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
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module CMake.Commands.File (file) where
import           CMake.Error                 (CmErrorKind (FatalError),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.ArgsParse (ArgsParser, natural, nullaryOpt,
                                              parseArgs, unaryArg, unaryOpt)
import           CMake.Interpreter.State     (CmBuiltinCommand, Interp, alt,
                                              currentScope, setVariable)
import           CMakeHs.Internal.Monad      (ifM)
import           CmSys.HardLink              (createHardLink)
import           CmSys.SymLink               (createSymbolicLink)
import           Control.Applicative         ((<|>))
import           Control.Monad.Catch         (catchIOError, throwM)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Base16      as B16 (encode)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                (($>))
import           Data.Label                  (mkLabels, set)
import           ParserT                     (item, tok)
import           System.Directory            (copyFile, createDirectory,
                                              createDirectoryIfMissing,
                                              doesDirectoryExist, doesPathExist,
                                              getFileSize,
                                              getSymbolicLinkTarget,
                                              listDirectory, removeFile,
                                              removePathForcibly, renamePath)
import           System.FilePath             (makeRelative, (</>))
import           System.IO                   (Handle, IOMode (..),
                                              SeekMode (AbsoluteSeek), hSeek,
                                              withBinaryFile, withFile)
import           System.IO.Error             (alreadyExistsErrorType,
                                              isAlreadyExistsError, mkIOError)

data FileReadArgs = FileReadArgs
                  { _rdFname :: ByteString
                  , _rdVar   :: ByteString
                  , _rdOff   :: Int
                  , _rdLim   :: Int
                  , _rdHex   :: Bool
                  } deriving Show
data FileRenameArgs = FileRenameArgs
                    { _rnOld       :: ByteString
                    , _rnNew       :: ByteString
                    , _rnRes       :: Maybe ByteString
                    , _rnNoReplace :: Bool
                    } deriving Show
data FileCreateLinkArgs = FileCreateLinkArgs
                        { _clOriginal  :: FilePath
                        , _clLinkName  :: FilePath
                        , _clRes       :: Maybe ByteString
                        , _clCopyOnErr :: Bool
                        , _clSymbolic  :: Bool
                        } deriving Show
data FileRelArgs = FileRelArgs
                 { _relVar  :: ByteString
                 , _relDir  :: FilePath
                 , _relFile :: FilePath
                 } deriving Show
data FileLockGuard = GuardFunction | GuardFile | GuardProcess deriving (Eq, Show)
data FileLockArgs = FileLockArgs
                  { _lkPath      :: ByteString
                  , _lkIsDir     :: Bool
                  , _lkIsRelease :: Bool
                  , _lkGuard     :: FileLockGuard
                  , _lkRes       :: Maybe ByteString
                  , _lkTo        :: Maybe Int
                  }
mkLabels [''FileReadArgs, ''FileRenameArgs, ''FileCreateLinkArgs, ''FileRelArgs, ''FileLockArgs]

file :: CmBuiltinCommand
file ("READ" : xs) cs = parseArgs "file(READ)" xs parser baseArgs cs >>= procRead
  where
    parser :: ArgsParser ByteString FileReadArgs ()
    parser = unaryArg "filename" rdFname (const item)
          >> unaryArg "variable" rdVar (const item)
          >> unaryOpt "OFFSET" rdOff natural
          >> unaryOpt "LIMIT" rdLim natural
          >> nullaryOpt "HEX" rdHex
    baseArgs = FileReadArgs mempty mempty 0 (-1) False
    procRead :: FileReadArgs -> Interp ()
    procRead FileReadArgs{_rdOff=0, _rdLim=(-1), _rdHex=False, ..} = liftIO (BS.readFile (BS.unpack _rdFname))
                                                                 >>= alt currentScope . setVariable _rdVar
    procRead FileReadArgs{..} = liftIO (withBinaryFile (BS.unpack _rdFname) ReadMode (seek'n'read _rdOff _rdLim))
                            >>= alt currentScope . setVariable _rdVar . perhapsHex _rdHex
    seek'n'read :: Int -> Int -> Handle -> IO ByteString
    seek'n'read off (-1) h = doSeek h off >> BS.hGetContents h
    seek'n'read off lim  h = doSeek h off >> BS.hGet h lim
    doSeek :: Handle -> Int -> IO ()
    doSeek h off = hSeek h AbsoluteSeek (fromIntegral off)
    perhapsHex :: Bool -> ByteString -> ByteString
    perhapsHex False = id
    perhapsHex True  = B16.encode

file ["WRITE"] cs = raiseArgumentCountError "file" cs
file ("WRITE" : f : strs) _ = () <$ liftIO (withFile (BS.unpack f) WriteMode (\h -> mapM_ (BS.hPutStr h) strs))

file ["APPEND"] cs = raiseArgumentCountError "file" cs
file ("APPEND" : f : strs) _ = () <$ liftIO (withFile (BS.unpack f) AppendMode (\h -> mapM_ (BS.hPutStr h) strs))

file ("MAKE_DIRECTORY" : dirs) _ = liftIO $ mapM_ (createDirectoryIfMissing True . BS.unpack) dirs

file ("REMOVE" : fs) _ = liftIO (mapM_ (removeFile . BS.unpack) fs) -- FIXME filter out and warn on empty strings
file ("REMOVE_RECURSE" : fs) _ = liftIO (mapM_ (removePathForcibly . BS.unpack) fs) -- FIXME filter out and warn on empty strings FIXME use something less lethal than removePathForcibly

file ("RENAME" : xs) cs = parseArgs "file(RENAME)" xs parser baseArgs cs >>= procRename
  where
    parser :: ArgsParser ByteString FileRenameArgs ()
    parser = unaryArg   "oldname"    rnOld (const item)
          >> unaryArg   "newname"    rnNew (const item)
          >> unaryOpt   "RESULT"     rnRes (const (Just <$> item))
          >> nullaryOpt "NO_REPLACE" rnNoReplace
    baseArgs = FileRenameArgs "" "" Nothing False
    procRename :: FileRenameArgs -> Interp ()
    procRename fra@FileRenameArgs{_rnRes=Just res}
        = catchIOError
            (procRename (set rnRes Nothing fra) $> "0")
            (\e -> pure if isAlreadyExistsError e
              then "NO_REPLACE"
              else BS.pack $ show e)
     >>= alt currentScope . setVariable res
    procRename fra@FileRenameArgs{_rnNoReplace=False, ..}
        = ifM (liftIO $ doesPathExist (BS.unpack _rnNew))
            (throwM $ mkIOError alreadyExistsErrorType "destination" Nothing (Just $ BS.unpack _rnNew))
            (procRename (set rnNoReplace True fra))
    procRename FileRenameArgs{..} = liftIO $ renamePath (BS.unpack _rnOld) (BS.unpack _rnNew)

file ["SIZE", f, o] _ = liftIO (getFileSize (BS.unpack f))
                    >>= alt currentScope . setVariable o . BS.pack . show
file ("SIZE" : _) cs = raiseArgumentCountError "file" cs

file ["READ_SYMLINK", f, o] _ = liftIO (getSymbolicLinkTarget (BS.unpack f))
                            >>= alt currentScope . setVariable o . BS.pack . show
file ("READ_SYMLINK" : _) cs = raiseArgumentCountError "file" cs

file ("CREATE_LINK" : xs) cs = parseArgs "file(CREATE_LINK)" xs parser baseArgs cs >>= procLink
  where
    parser :: ArgsParser ByteString FileCreateLinkArgs ()
    parser = unaryArg   "original"      clOriginal (const (BS.unpack <$> item))
          >> unaryArg   "linkname"      clLinkName (const (BS.unpack <$> item))
          >> unaryOpt   "RESULT"        clRes (const (Just <$> item))
          >> nullaryOpt "COPY_ON_ERROR" clCopyOnErr
          >> nullaryOpt "SYMBOLIC"      clSymbolic
    baseArgs = FileCreateLinkArgs mempty mempty Nothing False False
    procLink :: FileCreateLinkArgs -> Interp ()
    procLink fcla@FileCreateLinkArgs{_clRes=Just res}
            = catchIOError
                (procLink (set clRes Nothing fcla) $> "0")
                (pure . BS.pack . show)
          >>= alt currentScope . setVariable res
    procLink fcla@FileCreateLinkArgs{_clCopyOnErr=True, ..} = catchIOError
                                                                (procLink $ set clCopyOnErr False fcla)
                                                                (const . liftIO $ copyPath _clOriginal _clLinkName)
    procLink FileCreateLinkArgs{..} = liftIO $ createLink _clSymbolic _clOriginal _clLinkName
    createLink :: Bool -> FilePath -> FilePath -> IO ()
    createLink True  = createSymbolicLink
    createLink False = createHardLink
    copyPath :: FilePath -> FilePath -> IO ()
    copyPath src dst = ifM (doesDirectoryExist src)
                           (copyDir src dst)
                           (copyFile src dst)
    copyDir :: FilePath -> FilePath -> IO ()
    copyDir src dst = createDirectory dst
                   >> listDirectory src
                  >>= mapM_ (\p -> copyPath (src </> p) (dst </> p))

-- file ("REAL_PATH" : _) _ = TODO

file ("RELATIVE_PATH" : xs) cs = parseArgs "file(RELATIVE_PATH)" xs parser baseArgs cs >>= procRel
  where
    parser :: ArgsParser ByteString FileRelArgs ()
    parser = unaryArg "variable"  relVar  (const item)
          >> unaryArg "directory" relDir  (const (BS.unpack <$> item))
          >> unaryArg "file"      relFile (const (BS.unpack <$> item))
    baseArgs = FileRelArgs mempty mempty mempty
    procRel :: FileRelArgs -> Interp ()
    procRel FileRelArgs{..} = alt currentScope . setVariable _relVar . BS.pack $ makeRelative _relDir _relFile

-- file ("TO_CMAKE_PATH" : _) _ = TODO
-- file ("TO_NATIVE_PATH" : _) _ = TODO

-- file ("DOWNLOAD" : _) _ = TODO -- needs options
-- file ("UPLOAD" : _) _ = TODO -- needs options

file ("LOCK" : xs) cs = parseArgs "file(LOCK)" xs parser baseArgs cs >>= procLock
  where
    parser :: ArgsParser ByteString FileLockArgs ()
    parser = unaryArg   "path"      lkPath (const item)
          >> nullaryOpt "DIRECTORY" lkIsDir
          >> nullaryOpt "RELEASE"   lkIsRelease
          >> unaryOpt   "GUARD"     lkGuard (const $  tok "FUNCTION" $> GuardFunction
                                                  <|> tok "FILE"     $> GuardFile
                                                  <|> tok "PROCESS"  $> GuardProcess)
          >> unaryOpt "RESULT_VARIABLE" lkRes (const (Just <$> item))
          >> unaryOpt "TIMEOUT" lkTo (fmap Just . natural)
    baseArgs = FileLockArgs mempty False False GuardProcess Nothing Nothing
    procLock :: FileLockArgs -> Interp ()
    procLock = undefined -- TODO needs system integration

-- file ("ARCHIVE_CREATE" : _) _ = TODO -- needs variadic
-- file ("ARCHIVE_EXTRACT" : _) _ = TODO -- needs variadic

file _ cs = liftIO $ () <$ cmFormattedError FatalError (Just "file") ["Unsupported/illegal operation"] cs
