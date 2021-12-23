-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `unset` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Unset (unset) where
import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (braced)
import           CMake.Interpreter.State     (CmBuiltinCommand, alt,
                                              currentScope, parentScope, sel,
                                              unsetVariable)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                (($>))
import           System.Environment          (unsetEnv)


unset :: CmBuiltinCommand
unset [name] _
  | Just envVar <- braced "ENV" name = liftIO $ unsetEnv (BS.unpack envVar) $> ()
  | otherwise = alt currentScope (unsetVariable name)
unset [_, "CACHE"] _ = pure () -- Cache does not exist in script mode
unset [name, "PARENT_SCOPE"] cs = do
    mps <- sel parentScope
    case mps of
      Just ps -> alt parentScope $ const $ Just $ unsetVariable name ps
      Nothing -> liftIO $ () <$ cmFormattedError AuthorWarning (Just "unset") [" Cannot unset \"", name, "\": current scope has no parent."] cs
unset _ callSite = raiseArgumentCountError "unset" callSite
