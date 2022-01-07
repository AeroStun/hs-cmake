-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Error management utilities
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Error (
  CmError(..),
  CmErrorKind(..),
  cmFormattedError,
  emitAuthorWarning,
  raiseFatalError,
  raiseArgumentCountError
  ) where
import           CMake.AST.Defs          (SourceLocation)
import           CMake.Interpreter.State (Interp)
import           Control.Monad.IO.Class  (liftIO)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import           System.IO               (stderr)

data CmError = CmError CmErrorKind String
data CmErrorKind = FatalError
                 | Error
                 | Warning
                 | AuthorWarning
                 | DeprecationWarning
                 deriving (Eq, Ord)

instance Show CmErrorKind where
  show FatalError         = "Error"
  show Error              = "Error"
  show Warning            = "Warning"
  show AuthorWarning      = "Warning (dev)"
  show DeprecationWarning = "Deprecation Warning"

suffix :: CmErrorKind -> Maybe ByteString
suffix AuthorWarning = Just "This warning is for project developers."
suffix _             = Nothing

cmFormattedError :: CmErrorKind -> Maybe ByteString -> [ByteString] -> SourceLocation -> IO ()
cmFormattedError kind source msg loc = BS.hPutStrLn stderr fmtd
  where
    fmtd :: ByteString
    fmtd = mconcat [ "CMake "
                   , BS.pack (show kind)
                   , " at ", BS.pack (show loc)
                   , maybe "" (\s -> mconcat [" (", s, ")"]) source
                   , ":\n  "
                   , mconcat (BS.append "  " <$> BS.lines (mconcat msg))
                   , maybe "" (BS.cons '\n') (suffix kind)]

emitAuthorWarning :: ByteString -> [ByteString] -> SourceLocation -> Interp ()
emitAuthorWarning f s cs = liftIO $ () <$ cmFormattedError AuthorWarning (Just f) s cs

raiseFatalError :: ByteString -> [ByteString] -> SourceLocation -> Interp a
raiseFatalError f s cs = liftIO $ cmFormattedError FatalError (Just f) s cs *> ioError (userError "")

raiseArgumentCountError :: ByteString -> SourceLocation -> Interp ()
raiseArgumentCountError fn = raiseFatalError fn [fn, " called with incorrect number of arguments"]
