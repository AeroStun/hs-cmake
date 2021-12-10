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
module CMake.Error (
  CmError(..),
  CmErrorKind(..),
  cmFormattedError,
  raiseArgumentCountError
  ) where
import           CMake.AST.Defs (SourceLocation)
import           System.IO      (hPutStrLn, stderr)

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

suffix :: CmErrorKind -> Maybe String
suffix AuthorWarning = Just "This warning is for project developers."
suffix _             = Nothing

cmFormattedError :: CmErrorKind -> Maybe String -> String -> SourceLocation -> IO ()
cmFormattedError kind source msg loc = hPutStrLn stderr fmtd
  where
    fmtd :: String
    fmtd = "CMake "
           ++ show kind
           ++ " at " ++ show loc
           ++ maybe "" (\s -> " (" ++ s ++ ")") source
           ++":\n  "
           ++ concat (("  "++) <$> lines msg)
           ++ maybe "" ('\n':) (suffix kind)

raiseArgumentCountError :: String -> SourceLocation -> IO (Maybe t)
raiseArgumentCountError funcName callSite = Nothing <$ doPrint
  where
    doPrint :: IO ()
    doPrint = cmFormattedError
                FatalError
                (Just funcName)
                (funcName ++ " called with incorrect number of arguments")
                callSite
