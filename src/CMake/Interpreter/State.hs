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
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.Interpreter.State (
  Interp,
  sel,
  alt,
  runInterp,
  execInterp,
  evalInterp,
  currentScope,
  parentScope,
  commands,
  evading,
  hasVariable,
  readVariable,
  altVariable,
  setVariable,
  unsetVariable,

  CmState(..),
  Evasion(..),
  CmScope(..),
  CmCommand(..),
  CmBuiltinCommand,
  emptyState,
  emptyScope,
  registerCommand,
  pushScope,
  popScope,
  enterLoop,
  exitLoop,
  ) where
import           CMake.AST.Defs
import           Control.Applicative      ((<|>))
import qualified Control.Monad.State      as S (gets, modify)
import           Control.Monad.State.Lazy (StateT (runStateT), evalStateT,
                                           execStateT)
import           Data.ByteString          (ByteString)
import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.HashMap.Strict      (HashMap, alter, delete, insert,
                                           member, (!?))
import qualified Data.HashMap.Strict      as HMap (empty)
import           Data.Maybe               (fromMaybe)

-- | CMake interpreter monad
type Interp a = StateT CmState IO a

-- | Selector for interpreter state fields
type InterpSel a = (Interp a, a -> Interp ())

sel :: InterpSel a -> Interp a
sel = fst

alt :: InterpSel a -> (a -> a) -> Interp ()
alt (gf,uf) mfun = do st <- gf
                      uf (mfun st)

runInterp :: Interp a -> CmState -> IO (a, CmState)
runInterp = runStateT

evalInterp :: Interp a -> CmState -> IO a
evalInterp = evalStateT

execInterp :: Interp a -> CmState -> IO CmState
execInterp = execStateT


-- | Interpreter state selector for the current scope
currentScope :: InterpSel CmScope
currentScope = (S.gets currentScope_, \x -> S.modify (\s -> s{currentScope_=x}))

-- | Interpreter state selector for the parent scope
parentScope :: InterpSel (Maybe CmScope)
parentScope = (S.gets (scopeParent . currentScope_), \x -> S.modify (\s -> s{currentScope_=(currentScope_ s){scopeParent=x}}))

-- | Interpreter state selector for commands
commands :: InterpSel CommandMap
commands = (S.gets commands_, \x -> S.modify (\s -> s{commands_=x}))

-- | Interpreter state selector for the evasion tracker
evading :: InterpSel Evasion
evading = (S.gets evading_, \x -> S.modify (\s -> s{evading_=x}))


-- | Interpreter state
data CmState = CmState { currentScope_ :: CmScope
                       , commands_     :: CommandMap
                       , evading_      :: Evasion
                       } deriving Show

data Evasion = None | Return | Break | Continue deriving (Eq, Show)

type VarMap = HashMap ByteString ByteString

-- | Variable scope
data CmScope = CmScope { scopeIntroducer :: Maybe CommandInvocation
                       , scopeVars       :: VarMap
                       , scopeParent     :: Maybe CmScope
                       , loopDepth       :: Int
                       } deriving Show

type CommandMap = HashMap (CI ByteString) CmCommand

-- | Invocable function
data CmCommand = CmFunction ScopeBlock
               | CmMacro    ScopeBlock
               | CmBuiltinCommand CmBuiltinCommand

instance Show CmCommand where
  show (CmFunction s)       = "(CmFunction " ++ show s ++ ")"
  show (CmMacro s)          = "(CmMacro " ++ show s ++ ")"
  show (CmBuiltinCommand _) = "<builtin>"

type CmBuiltinCommand =  [ByteString] -> SourceLocation -> Interp ()

-- | empty state
emptyState :: CmState
emptyState = CmState emptyScope HMap.empty None

-- | empty scope
emptyScope :: CmScope
emptyScope = CmScope Nothing HMap.empty Nothing 0

-- | checks the existence of a variable in the current scope and above
hasVariable :: ByteString -> CmScope -> Bool
hasVariable name CmScope{scopeVars, scopeParent} = name `member` scopeVars || searchNext scopeParent
  where
    searchNext :: Maybe CmScope -> Bool
    searchNext Nothing      = False
    searchNext (Just scope) = hasVariable name scope

-- | read a variable's value in the current scope and above
readVariable :: ByteString -> CmScope -> Maybe ByteString
readVariable name CmScope{scopeVars, scopeParent} = (scopeVars !? name) <|> searchNext scopeParent
   where
     searchNext :: Maybe CmScope -> Maybe ByteString
     searchNext Nothing      = Nothing
     searchNext (Just scope) = readVariable name scope

-- | set a variable's value in the provided scope
setVariable :: ByteString -> ByteString -> CmScope -> CmScope
setVariable name value s@CmScope{scopeVars} =  s{scopeVars=insert name value scopeVars}

-- | mutate a variable's value in the provided scope
-- an empty string is provided if the variable does not exist
altVariable :: ByteString -> (ByteString -> ByteString) -> CmScope -> CmScope
altVariable name f s@CmScope{scopeVars} =  s{scopeVars=alter (Just . f . fromMaybe "") name scopeVars}

-- | unset a variable's value in the current scope
unsetVariable :: ByteString -> CmScope -> CmScope
unsetVariable name s@CmScope{scopeVars} =  s{scopeVars=delete name scopeVars}

-- | registers a command by name in the state
registerCommand :: ByteString -> CmCommand -> CmState -> CmState
registerCommand name cmd s@CmState{commands_} = s{commands_=insert (CI.mk name) cmd commands_}

pushScope :: CmScope -> CmState -> CmState
pushScope c t@CmState{currentScope_} = t{currentScope_=c{scopeParent=Just currentScope_}}

popScope :: CmState -> CmState
popScope s@CmState{currentScope_=CmScope{scopeParent=Just parent}} = s{currentScope_=parent, evading_=None}
popScope _ = error "Internal error: tried to pop top-level scope"

enterLoop :: CmState -> CmState
enterLoop s@CmState{currentScope_=CmScope{loopDepth}} = s{currentScope_=(currentScope_ s){loopDepth= loopDepth + 1}}

exitLoop :: CmState -> CmState
exitLoop s@CmState{evading_=Break}                   = exitLoop s{evading_=None}
exitLoop s@CmState{evading_=Continue}                = exitLoop s{evading_=None}
exitLoop s@CmState{currentScope_=CmScope{loopDepth}} = s{currentScope_=(currentScope_ s){loopDepth= loopDepth - 1}}
