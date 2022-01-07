{-# LANGUAGE CPP #-}
module CmSys.SymLink (createSymbolicLink) where
#if defined(mingw32_HOST_OS)
import           System.Directory (createDirectoryLink, createFileLink,
                                   doesDirectoryExist)
#else
import           System.Directory (createFileLink)
#endif

createSymbolicLink :: FilePath -> FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createSymbolicLink target linkName = ifM (doesDirectoryExist target)
                                         (createDirectoryLink target linkName)
                                         (createFileLink target linkName)
#else
createSymbolicLink = createFileLink
#endif
