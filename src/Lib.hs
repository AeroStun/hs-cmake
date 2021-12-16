module Lib (
  run,
  run_,
  runFile
  ) where
import qualified CMake.AST.Parser        as AST
import           CMake.Interpreter       (cmPrelude, processFile)
import           CMake.Interpreter.State (CmState (..), setVariable)
import           Data.Functor            (void, ($>))
import           System.Directory        (getCurrentDirectory)
import           Text.Trifecta           (parseFromFile, parseString)
import           Text.Trifecta.Result    (ErrInfo (..), Result (..))

run :: String -> IO ()
run s = case parseString AST.file mempty s of
    Success f                  -> processFile f cmPrelude $> ()
    Failure ErrInfo{_errDoc=e} -> print e

run_ :: String -> IO (Maybe CmState)
run_ s = case parseString AST.file mempty s of
    Success f                  -> processFile f cmPrelude
    Failure ErrInfo{_errDoc=e} -> error $ show e

runFile :: String -> IO ()
runFile s = do
   result <- parseFromFile AST.file s
   cwd <- getCurrentDirectory
   case result of
     Just program -> void $ processFile program $ setVars cwdVars cwd cmPrelude
     Nothing      -> pure ()
  where
    setVars :: [String] -> String -> CmState -> CmState
    setVars vs val st = foldl (\p@CmState{currentScope=ps} v -> p{currentScope=setVariable v val ps}) st vs
    cwdVars :: [String]
    cwdVars = ["CMAKE_SOURCE_DIR", "CMAKE_BINARY_DIR", "CMAKE_CURRENT_SOURCE_DIR", "CMAKE_CURRENT_BINARY_DIR"]
