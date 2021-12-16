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

module CMake.Commands.File (file) where
import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError,
                                          raiseArgumentCountError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmState (..),
                                          setVariable)
import           System.Directory        (getFileSize, removeFile)
import           System.IO               (IOMode (..), hPutStr, withFile)

file :: CmBuiltinCommand
file ["READ", f, var] _ s@CmState{currentScope=ps} = Just . (\c -> s{currentScope=setVariable var c ps}) <$> readFile f
file ["WRITE"] cs _ = raiseArgumentCountError "file" cs
file ("WRITE" : f : strs) _ s = Just s <$ withFile f WriteMode (\h -> mapM_ (hPutStr h) strs)
file ["APPEND"] cs _ = raiseArgumentCountError "file" cs
file ("APPEND" : f : strs) _ s = Just s <$ withFile f AppendMode (\h -> mapM_ (hPutStr h) strs)
file ("REMOVE" : fs) _ s = Just s <$ mapM_ removeFile fs
file ["SIZE", f, o] _ s@CmState{currentScope=ps} = Just . (\v -> s{currentScope=setVariable o v ps}) . show <$> getFileSize f
file ("SIZE" : _ : _ : _) cs _ = raiseArgumentCountError "file" cs

file _ cs _ = Nothing <$ cmFormattedError FatalError (Just "file") "Unsupported/illegal operation" cs
