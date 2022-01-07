-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `include` command
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module CMake.Commands.Include (include) where

import           CMake.AST.Defs              (SourceLocation)
import qualified CMake.AST.Parser            as AST (file)
import           CMake.Error                 (emitAuthorWarning,
                                              raiseArgumentCountError)
import           CMake.Interpreter           (processFile)
import           CMake.Interpreter.ArgsParse
import           CMake.Interpreter.State     (CmBuiltinCommand, Interp, alt,
                                              currentScope, readVariable, sel,
                                              setVariable)
import           CMake.List                  (splitCmList)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Label                  (mkLabel)
import           Data.Maybe                  (fromMaybe)
import           ParserT                     (item)
import           System.Directory            (findFile, getPermissions,
                                              readable)
import           System.IO.Error             (catchIOError)
import           Text.Trifecta.Parser        (parseFromFile)

data IncludeArgs = IncludeArgs
                 { _morf          :: ByteString
                 , _opt           :: Bool
                 , _resVar        :: Maybe ByteString
                 , _noPolicyScope :: Bool
                 } deriving Show
mkLabel ''IncludeArgs

include :: CmBuiltinCommand
include [] cs = raiseArgumentCountError "include" cs
include xs cs = parseArgs "include" xs parser baseArgs cs >>= flip include' cs
  where
    parser :: ArgsParser ByteString IncludeArgs ()
    parser = unaryArg "file|module" morf (const item)
          >> nullaryOpt "OPTIONAL" opt
          >> unaryOpt "RESULT_VARIABLE" resVar (const $ Just <$> item)
          >> nullaryOpt "NO_POLICY_SCOPE" noPolicyScope
    baseArgs = IncludeArgs mempty False Nothing False


include' :: IncludeArgs -> SourceLocation -> Interp ()
include' IncludeArgs{_morf=""} cs = emitAuthorWarning "include" ["include() given empty file name (ignored)."] cs
include' IncludeArgs{..} _ = findInModulePath (BS.append _morf ".cmake") >>= (\f -> include'' f _opt _resVar) . fromMaybe (BS.unpack _morf)

findInModulePath :: ByteString -> Interp (Maybe FilePath)
findInModulePath f = sel currentScope
                 >>= (\p -> liftIO $ findFile (BS.unpack <$> splitCmList (fromMaybe "" p)) (BS.unpack f))
                   . readVariable "CMAKE_MODULE_PATH"

include'' :: FilePath -> Bool -> Maybe ByteString -> Interp ()
include'' f o mres = do
     canRead <- liftIO $ catchIOError (readable <$> getPermissions f) (const $ pure False)
     case (canRead, o, mres) of
        (True, _, _) -> do
           result <- parseFromFile AST.file f
           case mres of
             Just res -> alt currentScope (setVariable res (BS.pack f))
             _        -> pure ()
           case result of
             Just program -> processFile program
             Nothing      -> pure () -- FIXME report parsing errors
        (False, False, _)       -> undefined -- FIXME raise fatal error
        (False, True, Just res) -> alt currentScope $ setVariable res "NOTFOUND"
        (False, True, Nothing)  -> pure ()
