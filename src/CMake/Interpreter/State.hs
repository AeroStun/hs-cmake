-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interpreter state type tree and associated accessors/manipulators
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Interpreter.State (
  CmState(..),
  CmScope(..),
  CmCommand(..),
  CmBuiltinCommand,
  emptyState,
  emptyScope,
  registerCommand,
  hasVariable,
  readVariable,
  setVariable,
  unsetVariable
  ) where
import           CMake.AST.Defs
import           Control.Applicative  ((<|>))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict  (HashMap, delete, insert, member, (!?))
import qualified Data.HashMap.Strict  as HMap (empty)

-- | interpreter state
data CmState = CmState { currentScope :: CmScope
                       , commands     :: CommandMap
                       } deriving Show

type VarMap = HashMap String String

-- | variable scope
data CmScope = CmScope { scopeIntroducer :: Maybe CommandInvocation
                       , scopeVars       :: VarMap
                       , scopeParent     :: Maybe CmScope
                       } deriving Show

type CommandMap = HashMap (CI String) CmCommand

-- | invocable function
data CmCommand = CmFunction ScopeBlock
               | CmBuiltinCommand CmBuiltinCommand

instance Show CmCommand where
  show (CmFunction s)       = "(CmFunction " ++ show s ++ ")"
  show (CmBuiltinCommand _) = "<builtin>"

type CmBuiltinCommand =  [String] -> SourceLocation -> CmState -> IO (Maybe CmState)

-- | empty state
emptyState :: CmState
emptyState = CmState emptyScope HMap.empty

-- | empty scope
emptyScope :: CmScope
emptyScope = CmScope Nothing HMap.empty Nothing

-- | checks the existence of a variable in the current scope and above
hasVariable :: String -> CmScope -> Bool
hasVariable name CmScope{scopeVars, scopeParent} = name `member` scopeVars || searchNext scopeParent
  where
    searchNext :: Maybe CmScope -> Bool
    searchNext Nothing      = False
    searchNext (Just scope) = hasVariable name scope

-- | read a variable's value in the current scope and above
readVariable :: String -> CmScope -> Maybe String
readVariable name CmScope{scopeVars, scopeParent} = (scopeVars !? name) <|> searchNext scopeParent
   where
     searchNext :: Maybe CmScope -> Maybe String
     searchNext Nothing      = Nothing
     searchNext (Just scope) = readVariable name scope

-- | set a variable's value in the current scope
setVariable :: String -> String -> CmScope -> CmScope
setVariable name value s@CmScope{scopeVars} =  s{scopeVars=insert name value scopeVars}

-- | unset a variable's value in the current scope
unsetVariable :: String -> CmScope -> CmScope
unsetVariable name s@CmScope{scopeVars} =  s{scopeVars=delete name scopeVars}

-- | registers a command by name in the state
registerCommand :: String -> CmCommand -> CmState -> CmState
registerCommand name cmd s@CmState{commands} = s{commands=insert (CI.mk name) cmd commands}
