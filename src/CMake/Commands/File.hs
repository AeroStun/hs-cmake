-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `file` command
------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.File (file) where
import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError,
                                          raiseArgumentCountError)
import           CMake.Interpreter.State (CmBuiltinCommand, alt, currentScope,
                                          setVariable)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Char8   as BS
import           System.Directory        (getFileSize, removeFile)
import           System.IO               (IOMode (..), withFile)

file :: CmBuiltinCommand
file ["READ", f, var] _ = do
    v <- liftIO $ BS.readFile (BS.unpack f)
    alt currentScope $ setVariable var v
file ["WRITE"] cs = raiseArgumentCountError "file" cs
file ("WRITE" : f : strs) _ = () <$ liftIO (withFile (BS.unpack f) WriteMode (\h -> mapM_ (BS.hPutStr h) strs))
file ["APPEND"] cs = raiseArgumentCountError "file" cs
file ("APPEND" : f : strs) _ = () <$ liftIO (withFile (BS.unpack f) AppendMode (\h -> mapM_ (BS.hPutStr h) strs))
file ("REMOVE" : fs) _ = () <$ liftIO (mapM_ (removeFile . BS.unpack) fs)
file ["SIZE", f, o] _ = do
    size <- liftIO $ getFileSize (BS.unpack f)
    alt currentScope $ setVariable o (BS.pack $ show size)
file ("SIZE" : _ : _ : _) cs = raiseArgumentCountError "file" cs

file _ cs = liftIO $ () <$ cmFormattedError FatalError (Just "file") ["Unsupported/illegal operation"] cs
