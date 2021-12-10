module Main where
import           Lib                (runFile)
import           System.Environment

main :: IO ()
main = getArgs >>= runFile . head
