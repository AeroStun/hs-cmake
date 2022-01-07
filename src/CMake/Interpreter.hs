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
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.Interpreter (processFile) where

import           CMake.AST.Defs
import           CMake.Cond.Eval             (evalCond)
import           CMake.Cond.Parser           (condition)
import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (applyFuncArgs, expandArguments)
import           CMake.Interpreter.State     (CmCommand (..), Evasion (..),
                                              Interp, alt, commands,
                                              currentScope, enterLoop, evading,
                                              exitLoop, needsSkip, popScope,
                                              popSkipRope, pushScope,
                                              readVariable, registerCommand,
                                              sel, setVariable, trackAscent,
                                              trackDive, trackMany, trackOne,
                                              unsetVariable)
import           CMake.List                  (splitCmList)
import           CMakeHs.Internal.Monad      (ifM)
import           CMakeHs.Internal.Numeric    (readMaybeInt)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Loops         (iterateUntilM)
import           Control.Monad.State.Lazy    (modify)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import           Data.Foldable               (foldlM)
import           Data.Function               (on)
import           Data.Functor                (void, ($>), (<&>))
import           Data.HashMap.Strict         ((!?))
import           Data.Maybe                  (fromMaybe)
import           ParserT                     (parseList)


processFile :: File -> Interp ()
processFile = processStatements

processStatements :: [Statement] -> Interp ()
processStatements xs = ifM needsSkip popSkipRope (pure 0)
                   >>= (\n -> trackDive
                           >> trackMany n
                           >> processStatements' (drop n xs)
                           >> trackAscent)
processStatements' :: [Statement] -> Interp ()
processStatements' = foldr ((>>) . (\s -> processStatement s >> trackOne)) (pure ())

processStatement :: Statement -> Interp ()
processStatement stmt = ifM (sel evading <&> (/= None)) (pure ()) case stmt of
    (InvocationStatement cmd) -> processInvocation cmd
    (ConditionalStatement (ConditionalChain first@(ConditionalBlock intro _) cmds outro)) ->
      foldlM processCondBlock Skipped (first : cmds) $> ()
    (MacroStatement b@(ScopeBlock (CommandInvocation _ ((name, _):_) _) _ _)) ->
      modify $ registerCommand name (CmMacro b)
    (MacroStatement _) -> liftIO $ fail "" <* putStrLn "DANGER: Macro unnamed" -- FIXME hoist to parser
    (FunctionStatement b@(ScopeBlock (CommandInvocation _ ((name, _):_) _) _ _)) ->
          modify $ registerCommand name (CmFunction b)
    (FunctionStatement _) -> liftIO $ fail "" <* putStrLn "DANGER: Function unnamed" -- FIXME hoist to parser
    (ForeachStatement (ScopeBlock (CommandInvocation _ [] cs) _ _)) -> raiseArgumentCountError "foreach" cs
    (ForeachStatement sb@(ScopeBlock (CommandInvocation _ args _) _ _)) -> do
        eargs <- expandArguments args
        modify enterLoop >> processForeach eargs sb >> modify exitLoop
    (WhileStatement condBlk _) -> modify enterLoop >> forwardWhile condBlk >> modify exitLoop
  where
    forwardWhile :: ConditionalBlock -> Interp CondBlockOutcome
    forwardWhile cb = iterateUntilM (== Skipped)
                        (\o -> processCondBlock o cb >>= ifM exitWhileTrans (pure Skipped) . pure)
                        Ran
    exitWhileTrans :: Interp Bool
    exitWhileTrans = sel evading <&> (\ev -> ev == Break || ev == Return)


data CondBlockOutcome = Ran
                      | Skipped
                      deriving (Eq, Show)

processCondBlock :: CondBlockOutcome -> ConditionalBlock -> Interp CondBlockOutcome
processCondBlock o c@(ConditionalBlock (CommandInvocation (Identifier introId) introArgs introLoc) stmts) = do
   evasion <- sel evading
   case (o, evasion) of
    (_, Continue)
      | CI.mk introId == "while" -> alt evading (const None) >> processCondBlock o c
    (Ran, _)
      | CI.mk introId /= "while" -> pure Ran
    (Skipped, _)
      | CI.mk introId == "else" -> processStatements stmts $> Ran
    _ -> do
      expArgs <- expandArguments introArgs
      case parseList condition expArgs of
        Nothing -> liftIO $ fail "" <* cmFormattedError FatalError (Just introId) ["Unknown arguments specified"] introLoc
        Just cond -> ifM (evalCond cond) (processStatements stmts $> Ran) (pure Skipped)

processForeach :: [ByteString] -> ScopeBlock -> Interp ()
processForeach [] (ScopeBlock (CommandInvocation _ _ cs) _ _) = raiseArgumentCountError "foreach" cs
processForeach [var, "RANGE", stopS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) =
     case readMaybeInt stopS of
       Just stop -> alt currentScope (unsetVariable var) >> processRangeForeach var (0, stop, 1) b
       Nothing   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
processForeach [var, "RANGE", startS, stopS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) =
     case (readMaybeInt startS, readMaybeInt stopS) of
       (Just start, Just stop) -> processRangeForeach var (start, stop, 1) b
       (Nothing, _)   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", startS, "'"] cs
       (_, Nothing)   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
processForeach [var, "RANGE", startS, stopS, stepS] b@(ScopeBlock (CommandInvocation _ _ cs) _ _) =
     case (readMaybeInt startS, readMaybeInt stopS, readMaybeInt stepS) of
       (Just start, Just stop, Just step) -> processRangeForeach var (start, stop, step) b
       (Nothing, _, _)   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", startS, "'"] cs
       (_, Nothing, _)   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stopS, "'"] cs
       (_, _, Nothing)   -> liftIO $ fail "" <* cmFormattedError FatalError (Just "foreach") ["foreach Invalid integer: '", stepS,  "'"] cs
processForeach (var : "IN" : "LISTS" :  xs) b = expandLists xs >>= \ls -> processListForeach var ls b
  where
    expandLists :: [ByteString] -> Interp [ByteString]
    expandLists [] = pure []
    expandLists ("ITEMS" : items) = pure items
    expandLists (l : ls) = sel currentScope >>= (\v -> (v++) <$> expandLists ls) . splitCmList . fromMaybe "" . readVariable l
processForeach (var : "IN" : "ITEMS" : xs) b = processListForeach var xs b
processForeach (_ : "IN" : "ZIP_LISTS" : _) _
    = error "Unimplemented feature ZIP_LISTS"
processForeach (var : vals) b = processListForeach var vals b

processListForeach :: ByteString -> [ByteString] -> ScopeBlock -> Interp ()
processListForeach _ [] _ = pure ()
processListForeach v (x : xs) b@(ScopeBlock _ stmts _) =
    ifM (sel evading <&> (== Continue))
      (alt evading (const None) >> processListForeach v xs b)
      (setLoopVar >> processStatements stmts >> processListForeach v xs b >> unsetLoopVar)
    where
      setLoopVar :: Interp ()
      setLoopVar = alt currentScope $ setVariable v x
      unsetLoopVar :: Interp ()
      unsetLoopVar = alt currentScope $ unsetVariable v

processRangeForeach :: ByteString -> (Int, Int, Int) -> ScopeBlock -> Interp ()
processRangeForeach _ (start, stop, _) _
  | start > stop = pure ()
processRangeForeach v l@(start, stop, step) b@(ScopeBlock _ stmts _) =
    ifM (sel evading <&> (== Continue))
      (alt evading (const None) >> processRangeForeach v l b)
      (setLoopVar >> processStatements stmts >> processRangeForeach v (start + step, stop, step) b >> unsetLoopVar)
  where
    setLoopVar :: Interp ()
    setLoopVar = alt currentScope $ setVariable v $ BS.pack $ show start
    unsetLoopVar :: Interp ()
    unsetLoopVar = alt currentScope $ unsetVariable v


processInvocation :: CommandInvocation -> Interp ()
processInvocation (CommandInvocation (Identifier name) args callSite) = do
  eargs <- expandArguments args
  cmds <- sel commands
  case cmds !? CI.mk name of
    Just (CmFunction (ScopeBlock (CommandInvocation _ (_:fArgs) _) stmts _))
      | ((<) `on` void) args fArgs -> raiseArgumentCountError name callSite
      | otherwise -> modify (pushScope (applyFuncArgs fArgs eargs))
                  >> alt currentScope (setVariable "CMAKE_CURRENT_FUNCTION" name)
                  >> processStatements stmts
                  >> modify popScope
    Just (CmMacro (ScopeBlock (CommandInvocation _ (_:fArgs) _) stmts _))
      | ((<) `on` void) args fArgs -> raiseArgumentCountError name callSite
      | otherwise -> processStatements stmts -- FIXME process macro args
    Just (CmBuiltinCommand bc) -> bc eargs callSite
    _ -> liftIO $ cmFormattedError FatalError (Just name) ["Unknown CMake command \"", name, "\"."] callSite >> fail ""
