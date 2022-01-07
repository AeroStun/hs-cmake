{-# LANGUAGE CPP #-}
module CmSys.HardLink (createHardLink) where

#if defined(mingw32_HOST_OS)
import qualified System.Win32.HardLink as Win32 (createHardLink)
#else
import           System.Posix.Files    (createLink)
#endif

createHardLink :: FilePath -> FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createHardLink = Win32.createHardLink
#else
createHardLink = createLink
#endif
  