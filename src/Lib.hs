module Lib (
  run,
  runFile
  ) where
import           CMake.AST.Defs       (File)
import qualified CMake.AST.Parser     as AST
import           CMake.Interpreter    (cmPrelude, processFile)
import           Data.Functor         (void, ($>))
import           Text.Trifecta        (parseFromFile, parseString)
import           Text.Trifecta.Result (ErrInfo (..), Result (..))

run :: String -> IO ()
run s = case parseString AST.file mempty s of
    Success f                  -> processFile f cmPrelude $> ()
    Failure ErrInfo{_errDoc=e} -> print e

runFile :: String -> IO ()
runFile s = do
   result <- parseFromFile AST.file s
   case result of
     Just program -> void $ processFile program cmPrelude
     Nothing      -> pure ()
