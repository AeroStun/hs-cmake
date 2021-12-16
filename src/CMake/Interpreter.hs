-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interpreter logic
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CMake.Interpreter (processFile, cmPrelude) where

import           CMake.AST.Defs
import           CMake.Commands
import           CMake.Cond.Eval             (evalCond)
import           CMake.Cond.Parser           (condition)
import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (applyFuncArgs, expandArguments)
import           CMake.Interpreter.State
import           CMake.List                  (splitCmList)
import           CMakeHs.Internal.Functor    ((<$$>), (<&&>))
import           CMakeHs.Internal.Monad      (ifM)
import           CMakeHs.Internal.Numeric    (readMaybeInt)
import           Control.Monad.Loops         (iterateUntilM)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import           Data.Foldable               (foldlM)
import           Data.Function               (on)
import           Data.Functor                (void, ($>))
import           Data.HashMap.Strict         ((!?))
import           Data.Maybe                  (fromJust)
import           ParserT                     (parseList)

foldMaybesM :: (Monad m, Foldable f) => (b -> a -> m (Maybe b)) -> b -> f a -> m (Maybe b)
foldMaybesM p i xs = runMaybeT $ foldlM ((MaybeT .) . p) i xs

processFile :: File -> CmState -> IO (Maybe CmState)
processFile = processStatements

processStatements :: [Statement] -> CmState -> IO (Maybe CmState)
processStatements cs s = foldMaybesM (flip processStatement) s cs

processStatement :: Statement -> CmState -> IO (Maybe CmState)
processStatement _ s@CmState{evading}
  | evading /= None = pure $ Just s
processStatement (InvocationStatement cmd) s = processInvocation cmd s
processStatement (ConditionalStatement (ConditionalChain first@(ConditionalBlock intro stmt) cmds outro)) s = do
    r <- foldMaybesM (flip processCondBlock) (s, Skipped) (first : cmds)
    return $ fst <$> r
processStatement (MacroStatement b@(ScopeBlock (CommandInvocation _ ((name, _):_) _) _ _)) s = pure $ Just $ registerCommand name (CmMacro b) s
processStatement (MacroStatement _) _ = putStrLn "DANGER: Macro unnamed" $> Nothing -- FIXME hoist to parser
processStatement (FunctionStatement b@(ScopeBlock (CommandInvocation _ ((name, _): _) _) _ _)) s = pure $ Just $ registerCommand name (CmFunction b) s
processStatement (FunctionStatement _) _ = putStrLn "DANGER: Function unnamed" $> Nothing -- FIXME hoist to parser
processStatement (ForeachStatement (ScopeBlock (CommandInvocation _ [] cs) _ _)) _ = raiseArgumentCountError "foreach" cs
processStatement (ForeachStatement sb@(ScopeBlock (CommandInvocation _ args cs) _ _)) s@CmState{currentScope} = do
    meargs <- expandArguments args currentScope
    case meargs of
       Just eargs -> exitLoop <$$> processForeach eargs sb (enterLoop s)
       Nothing -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["Unknown arguments specified"] cs

processStatement (WhileStatement condBlk _) s = (exitLoop . fst)
                                           <$$> iterateUntilM (maybe True (\(CmState{evading}, o) -> o == Skipped || evading == Break || evading == Return))
                                                              (processCondBlock condBlk . fromJust)
                                                              (Just (enterLoop s, Ran))

data CondBlockOutcome = Ran | Skipped deriving (Eq, Show)

processCondBlock :: ConditionalBlock -> (CmState, CondBlockOutcome) -> IO (Maybe (CmState, CondBlockOutcome))
processCondBlock c@(ConditionalBlock (CommandInvocation (Identifier introId) _ _) _) (s@CmState{evading=Continue}, o)
  | CI.mk introId == "while" = processCondBlock c (s{evading=None}, o)
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) _ _) _) (s, Ran)
  | CI.mk introId /= "while" = pure $ Just (s, Ran)
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) _ _) stmts) (s, Skipped)
  | CI.mk introId == "else" = (,Ran) <$$> processStatements stmts s
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) introArgs introLoc) stmts) (s@CmState{currentScope}, _) = do
  expArgs <- expandArguments introArgs currentScope
  case expArgs >>= parseList condition of
    Nothing -> cmFormattedError FatalError (Just introId) ["Unknown arguments specified"] introLoc $> Nothing
    Just cond -> ifM (evalCond cond s) ((,Ran) <$$> processStatements stmts s) (return $ Just (s, Skipped))

processForeach :: [ByteString] -> ScopeBlock -> CmState -> IO (Maybe CmState)
processForeach [] (ScopeBlock (CommandInvocation _ _ cs) _ _) _ = raiseArgumentCountError "foreach" cs
processForeach [var, "RANGE", stopS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) s =
     case readMaybeInt stopS of
       Just stop -> processRangeForeach var (0, stop, 1) b s
               <&&> (\st@CmState{currentScope} -> st{currentScope=unsetVariable var currentScope})
       Nothing   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
processForeach [var, "RANGE", startS, stopS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) s =
     case (readMaybeInt startS, readMaybeInt stopS) of
       (Just start, Just stop) -> processRangeForeach var (start, stop, 1) b s
       (Nothing, _)   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", startS, "'"] cs
       (_, Nothing)   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
processForeach [var, "RANGE", startS, stopS, stepS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) s =
     case (readMaybeInt startS, readMaybeInt stopS, readMaybeInt stepS) of
       (Just start, Just stop, Just step) -> processRangeForeach var (start, stop, step) b s
       (Nothing, _, _)   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", startS, "'"] cs
       (_, Nothing, _)   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
       (_, _, Nothing)   -> Nothing <$ cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stepS,  "'"] cs
processForeach (var : "IN" : "LISTS" :  xs) b s@CmState{currentScope} = processListForeach var (expandLists xs) b s
  where
    expandLists :: [ByteString] -> [ByteString]
    expandLists [] = []
    expandLists ("ITEMS" : items) = items
    expandLists (l : ls) = maybe [] splitCmList (readVariable l currentScope) ++ expandLists ls
processForeach (var : "IN" : "ITEMS" : xs) b s = processListForeach var xs b s
processForeach (_ : "IN" : "ZIP_LISTS" : _) _ _
    = error "Unimplemented feature ZIP_LISTS"
processForeach (var : vals) b s = processListForeach var vals b s

processListForeach :: ByteString -> [ByteString] -> ScopeBlock -> CmState -> IO (Maybe CmState)
processListForeach _ [] _ s = pure $ Just s
processListForeach v xs b s@CmState{evading=Continue} = processListForeach v xs b s{evading=None}
processListForeach v (x : xs) b@(ScopeBlock _ stmts _) s =
    (processStatements stmts (setLoopVar s) >>= forward) <&&> unsetLoopVar
    where
      setLoopVar :: CmState -> CmState
      setLoopVar st@CmState{currentScope} = st{currentScope=setVariable v x currentScope}
      forward :: Maybe CmState -> IO (Maybe CmState)
      forward = maybe (pure Nothing) (processListForeach v xs b)
      unsetLoopVar :: CmState -> CmState
      unsetLoopVar st@CmState{currentScope=endScope} = st{currentScope=unsetVariable v endScope}

processRangeForeach :: ByteString -> (Int, Int, Int) -> ScopeBlock -> CmState -> IO (Maybe CmState)
processRangeForeach v l@(start, stop, step) b@(ScopeBlock _ stmts _) s@CmState{evading}
  | evading == Continue = processRangeForeach v l b s{evading=None}
  | start > stop = pure $ Just s
  | otherwise = (processStatements stmts (setLoopVar s) >>= forward) <&&> unsetLoopVar
  where
    setLoopVar :: CmState -> CmState
    setLoopVar st@CmState{currentScope} = st{currentScope=setVariable v (BS.pack $ show start) currentScope}
    forward :: Maybe CmState -> IO (Maybe CmState)
    forward = maybe (pure Nothing) (processRangeForeach v (start + step, stop, step) b)
    unsetLoopVar :: CmState -> CmState
    unsetLoopVar st@CmState{currentScope=endScope} = st{currentScope=unsetVariable v endScope}

processInvocation :: CommandInvocation -> CmState -> IO (Maybe CmState)
processInvocation (CommandInvocation (Identifier name) args callSite) s@CmState{commands, currentScope} = do
  meargs <- expandArguments args currentScope
  case commands !? CI.mk name of
    Just (CmFunction (ScopeBlock (CommandInvocation _ (_:fArgs) _) stmts _))
      | ((<) `on` void) args fArgs -> raiseArgumentCountError name callSite
      | Just eargs <- meargs -> popScope <$$> processStatements stmts (pushScope (applyFuncArgs fArgs eargs) s)
    Just (CmMacro (ScopeBlock (CommandInvocation _ (_:fArgs) _) stmts _))
      | ((<) `on` void) args fArgs -> raiseArgumentCountError name callSite
      | otherwise -> processStatements stmts s -- FIXME process macro args
    Just (CmBuiltinCommand bc)
      | Just eargs <- meargs -> bc eargs callSite s
      | otherwise -> pure Nothing
    _ -> Nothing <$ cmFormattedError FatalError (Just name) ["Unknown CMake command \"", name, "\"."] callSite

cmPrelude :: CmState
cmPrelude = registerCommand "cmake_policy" (CmBuiltinCommand cmakePolicy)
          $ registerCommand "file" (CmBuiltinCommand file)
          $ registerCommand "math" (CmBuiltinCommand math)
          $ registerCommand "string" (CmBuiltinCommand string)
          $ registerCommand "message" (CmBuiltinCommand simpleMessage)
          $ registerCommand "dbg_printvar" (CmBuiltinCommand dbgPrintvar)
          $ registerCommand "unset" (CmBuiltinCommand unset)
          $ registerCommand "set" (CmBuiltinCommand set)
          $ registerCommand "continue" (CmBuiltinCommand cmContinue)
          $ registerCommand "break" (CmBuiltinCommand cmBreak)
          $ registerCommand "return" (CmBuiltinCommand cmReturn) emptyState
  where
    dbgPrintvar :: CmBuiltinCommand
    dbgPrintvar [name] _ s@CmState{currentScope} = BS.putStrLn (mconcat [name, ": ", maybe "<unset>" (\v -> mconcat ["\"", v, "\""]) (readVariable name currentScope)]) $> Just s
    dbgPrintvar _ caller _ = pure Nothing

    simpleMessage :: CmBuiltinCommand
    simpleMessage [] caller _ = pure Nothing
    simpleMessage msgs _ s    = BS.putStrLn (mconcat msgs) $> Just s
