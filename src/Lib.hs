-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interpreter drivers for Main and interactive
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Lib (
  run,
  run_,
  runFile
  ) where
import qualified CMake.AST.Parser        as AST
import           CMake.InterpPrelude     (cmPrelude)
import           CMake.Interpreter       (processFile)
import           CMake.Interpreter.State (CmState (..), Interp, alt,
                                          currentScope, evalInterp, execInterp,
                                          setVariable)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import           Data.Functor            (($>))
import           System.Directory        (getCurrentDirectory)
import           Text.Trifecta           (parseFromFile, parseString)
import           Text.Trifecta.Result    (ErrInfo (..), Result (..))

run :: String -> IO ()
run s = case parseString AST.file mempty s of
    Success f                  -> evalInterp (processFile f) cmPrelude $> ()
    Failure ErrInfo{_errDoc=e} -> print e

run_ :: String -> IO CmState
run_ s = case parseString AST.file mempty s of
    Success f                  -> execInterp (processFile f) cmPrelude
    Failure ErrInfo{_errDoc=e} -> error $ show e

runFile :: String -> IO ()
runFile s = do
   result <- parseFromFile AST.file s
   cwd <- BS.pack <$> getCurrentDirectory
   case result of
     Just program -> evalInterp (setVars cwdVars cwd >> processFile program) cmPrelude
     Nothing      -> pure ()
  where
    setVars :: [ByteString] -> ByteString -> Interp ()
    setVars vs val = alt currentScope (\sc -> foldl (\ps v -> setVariable v val ps) sc vs)
    cwdVars :: [ByteString]
    cwdVars = ["CMAKE_SOURCE_DIR", "CMAKE_BINARY_DIR", "CMAKE_CURRENT_SOURCE_DIR", "CMAKE_CURRENT_BINARY_DIR"]
