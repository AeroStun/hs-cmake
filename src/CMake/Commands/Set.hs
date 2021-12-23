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
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Set (set) where
import           CMake.AST.Defs              (SourceLocation)
import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (braced)
import           CMake.Interpreter.State     (CmBuiltinCommand, CmScope,
                                              setVariable, currentScope, alt, sel, parentScope)
import           CMake.List                  (joinCmList)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                (($>))
import qualified System.Environment          as Sys (setEnv)

set :: CmBuiltinCommand
set [] cs = raiseArgumentCountError "set" cs
set (bname : values) callSite
  | Just envVar <- braced "ENV" bname = liftIO $ setEnv envVar values callSite $> ()
set [name] _  = alt currentScope $ setVariable name ""
set (name : values) _
  | last values /= "PARENT_SCOPE" = alt currentScope $ set' name values
set (name : values) cs = do
    mps <- sel parentScope
    case mps of
      Just ps -> alt parentScope $ const $ Just $ set' name (init values) ps
      Nothing -> liftIO $ () <$ cmFormattedError AuthorWarning (Just "set") [" Cannot set \"", name, "\": current scope has no parent."] cs

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
