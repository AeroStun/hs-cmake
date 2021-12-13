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
import           CMake.Error                 (CmError (..), CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (applyFuncArgs, expandArguments)
import           CMake.Interpreter.State
import           Control.Monad.Loops         (iterateUntilM)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import qualified Data.CaseInsensitive        as CI
import           Data.Foldable               (foldlM)
import           Data.Function               (on)
import           Data.Functor                (void, ($>))
import           Data.HashMap.Strict         ((!?))
import           Data.Maybe                  (fromJust)
import           ParserT                     (parseList)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) m v = fmap m <$> v

foldMaybesM :: (Monad m, Foldable f) => (b -> a -> m (Maybe b)) -> b -> f a -> m (Maybe b)
foldMaybesM p i xs = runMaybeT $ foldlM ((MaybeT .) . p) i xs

processFile :: File -> CmState -> IO (Maybe CmState)
processFile = processStatements

processStatements :: [Statement] -> CmState -> IO (Maybe CmState)
processStatements cs s = foldMaybesM (flip processStatement) s cs

processStatement :: Statement -> CmState -> IO (Maybe CmState)
processStatement (InvocationStatement cmd) s = processInvocation cmd s
processStatement (ConditionalStatement (ConditionalChain first@(ConditionalBlock intro stmt) cmds outro)) s = do
    r <- foldMaybesM (flip processCondBlock) (s, Skipped) (first : cmds)
    return $ fst <$> r
processStatement (MacroStatement b@(ScopeBlock (CommandInvocation _ ((name, _):_) _) _ _)) s = pure $ Just $ registerCommand name (CmFunction b) s
processStatement (MacroStatement _) _ = putStrLn "DANGER: Macro unnamed" $> Nothing -- FIXME hoist to parser
processStatement (FunctionStatement b@(ScopeBlock (CommandInvocation _ ((name, _): _) _) _ _)) s = pure $ Just $ registerCommand name (CmFunction b) s
processStatement (FunctionStatement _) _ = putStrLn "DANGER: Function unnamed" $> Nothing -- FIXME hoist to parser
processStatement (ForeachStatement _) s = putStrLn "Unimplemented foreach command" $> Nothing
processStatement (WhileStatement condBlk _) s = fst <$$> iterateUntilM (maybe True ((==Skipped). snd)) (processCondBlock condBlk . fromJust) (Just (s, Ran))

data CondBlockOutcome = Ran | Skipped deriving (Eq, Show)

processCondBlock :: ConditionalBlock -> (CmState, CondBlockOutcome) -> IO (Maybe (CmState, CondBlockOutcome))
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) _ _) _) (s, Ran)
  | CI.mk introId /= "while" = pure $ Just (s, Ran)
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) _ _) stmts) (s, Skipped)
  | CI.mk introId == "else" = (,Ran) <$$> processStatements stmts s
processCondBlock (ConditionalBlock (CommandInvocation (Identifier introId) introArgs introLoc) stmts) (s@CmState{currentScope}, _) =
  case expandArguments introArgs currentScope >>= parseList condition of
    Nothing -> cmFormattedError FatalError (Just introId) "Unknown arguments specified" introLoc $> Nothing
    Just cond -> do
        condResult <- evalCond cond s
        if condResult then (,Ran) <$$> processStatements stmts s else return $ Just (s, Skipped)

processInvocation :: CommandInvocation -> CmState -> IO (Maybe CmState)
processInvocation (CommandInvocation (Identifier name) args callSite) s@CmState {commands, currentScope} =
  case commands !? CI.mk name of
    Just (CmFunction (ScopeBlock (CommandInvocation (Identifier cfName) (_:fArgs) _) stmts _))
      | ((<) `on` void) args fArgs -> raiseArgumentCountError name callSite
      | otherwise ->
        let state = if CI.mk cfName == CI.mk "function" -- functions introduce scope, macros do not
                    then s{currentScope=applyFuncArgs fArgs args emptyScope{scopeParent=Just currentScope}}
                    else s
        -- FIXME process macro args
        in processStatements stmts state
    Just (CmBuiltinCommand bc) ->
      case expandArguments args currentScope of
        Just eargs -> bc eargs callSite s
        Nothing    -> pure Nothing
    _ -> Nothing <$ cmFormattedError FatalError (Just name) ("Unknown CMake command \"" ++ name ++ "\".") callSite

cmPrelude :: CmState
cmPrelude = registerCommand "cmake_policy" (CmBuiltinCommand cmakePolicy)
          $ registerCommand "message" (CmBuiltinCommand simpleMessage)
          $ registerCommand "dbg_printvar" (CmBuiltinCommand dbgPrintvar)
          $ registerCommand "unset" (CmBuiltinCommand unset)
          $ registerCommand "set" (CmBuiltinCommand set) emptyState
  where
    dbgPrintvar :: CmBuiltinCommand
    dbgPrintvar [name] _ s@CmState{currentScope} = putStrLn (name ++ ": " ++ maybe "<unset>" (\v -> "\""++v++"\"") (readVariable name currentScope)) $> Just s
    dbgPrintvar _ caller _ = pure Nothing

    simpleMessage :: CmBuiltinCommand
    simpleMessage [] caller _ = pure Nothing
    simpleMessage msgs _ s    = putStrLn (concat msgs) $> Just s
