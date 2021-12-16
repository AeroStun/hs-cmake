-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `set` command
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Set (set) where
import           CMake.AST.Defs              (SourceLocation)
import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (braced)
import           CMake.Interpreter.State     (CmBuiltinCommand, CmScope (..),
                                              CmState (..), setVariable)
import           CMake.List                  (joinCmList)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                (($>))
import qualified System.Environment          as Sys (setEnv)

set :: CmBuiltinCommand
set [] callSite _ = raiseArgumentCountError "set" callSite
set (bname : values) callSite s
  | Just envVar <- braced "ENV" bname = setEnv envVar values callSite $> Just s
set [name] _ s@CmState{currentScope} = pure $ Just s{currentScope=setVariable name "" currentScope}
set (name : values) _ s@CmState{currentScope}
  | last values /= "PARENT_SCOPE" =
      pure $ Just s{currentScope=set' name values currentScope}
set (name : _) callSite s@CmState{currentScope=CmScope{scopeParent=Nothing}} =
  Just s <$ cmFormattedError AuthorWarning (Just "set") [" Cannot set \"", name, "\": current scope has no parent."] callSite
set (name : values) _ s@CmState{currentScope=CmScope{scopeParent=Just scope}} =
  pure $ Just s{currentScope=(currentScope s){scopeParent=Just $ set' name values scope}}

set' :: ByteString -> [ByteString] -> CmScope -> CmScope
set' name values = setVariable name (joinCmList values)

setEnv :: ByteString -> [ByteString] -> SourceLocation -> IO ()
setEnv name [] _       = Sys.setEnv (BS.unpack name) ""
setEnv name [value] _  = Sys.setEnv (BS.unpack name) (BS.unpack value)
setEnv name (value : sndVal : _) callSite = setEnv name [value] callSite <* warn
  where
    warn :: IO ()
    warn = cmFormattedError AuthorWarning (Just "set") warning callSite
    warning :: [ByteString]
    warning = ["Only the first value argument is used when setting an environment variable.\nArgument '", sndVal, "' and later are unused."]

