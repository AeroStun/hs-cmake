-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `string` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module CMake.Commands.String (string) where
import           CMake.Error                (CmErrorKind (..), cmFormattedError,
                                             raiseArgumentCountError)
import           CMake.Interpreter.State    (CmBuiltinCommand, Interp, alt,
                                             altVariable, currentScope,
                                             setVariable)
import           CMakeHs.Internal.Numeric   (readMaybeInt)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as BS16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Search     as BSS
import           Data.Char                  (isSpace, toLower, toUpper)
import           Data.Maybe                 (fromMaybe, listToMaybe)

string :: CmBuiltinCommand
string [] cs = raiseArgumentCountError "string" cs

-- SEARCH AND REPLACE
string ["FIND", haystack, needle, outputVar] _ = alt currentScope $ setVariable outputVar (BS.pack $ show $ findSubstring needle haystack False)
string ["FIND", haystack, needle, outputVar, "REVERSE"] _ = alt currentScope $setVariable outputVar (BS.pack $ show $ findSubstring needle haystack True)
string ("FIND" : _) cs = raiseArgumentCountError "string" cs
string ("REPLACE" : m : r : o : x : xs) _ = alt currentScope $ setVariable o (LBS.toStrict $ BSS.replace m r $ BS.concat (x : xs))

-- MANIPULATION
string ("APPEND" : var : inputs) _ = alt currentScope $ altVariable var (`BS.append` BS.concat inputs)
string ("PREPEND" : var : inputs) _ = alt currentScope $ altVariable var (BS.concat inputs `BS.append`)
string ("CONCAT" : stringVar: inputs) _ = alt currentScope $ setVariable stringVar (BS.concat inputs)
string ("JOIN" : glue : stringVar: inputs) _ = alt currentScope $ setVariable stringVar (BS.intercalate glue inputs)
string ["TOLOWER", str, stringVar] _ = alt currentScope $ setVariable stringVar (BS.map toLower str)
string ["TOUPPER", str, stringVar] _ = alt currentScope $ setVariable stringVar (BS.map toUpper str)
string ["LENGTH", str, output] _ = alt currentScope $ setVariable output (BS.pack $ show $ BS.length str)
string ["SUBSTRING", str, beginS, lenS, o] cs =
    case (readMaybeInt beginS, readMaybeInt lenS) of
      (Just begin, _)
        | begin < 0 || begin >= BS.length str -> liftIO $ fail "" <* cmFormattedError FatalError (Just "string") ["string begin index is out of range"] cs
      (Just begin, Just (-1)) -> alt currentScope $ setVariable o (BS.drop begin str)
      (Just begin, Just len) -> alt currentScope $ setVariable o (BS.take len $ BS.drop begin str)
      _ -> raiseArgumentCountError "string" cs
string ["STRIP", str, o] _ = alt currentScope $ setVariable o (fst $ BS.breakEnd isSpace $ snd $ BS.span isSpace str)
string ["REPEAT", str, countS, outputVar] cs =
    case readMaybeInt countS of
      Just count
        | count >= 0 -> alt currentScope $ setVariable outputVar (BS.concat $ replicate count str)
      _ -> liftIO $ fail "" <* cmFormattedError FatalError (Just "string") ["repeat count is not a positive number."] cs


-- COMPARISON
string ["COMPARE", "LESS",          l, r, o] _ = stringCompare (<)  l r o
string ["COMPARE", "GREATER",       l, r, o] _ = stringCompare (>)  l r o
string ["COMPARE", "EQUAL",         l, r, o] _ = stringCompare (==) l r o
string ["COMPARE", "NOTEQUAL",      l, r, o] _ = stringCompare (/=) l r o
string ["COMPARE", "LESS_EQUAL",    l, r, o] _ = stringCompare (<=) l r o
string ["COMPARE", "GREATER_EQUAL", l, r, o] _ = stringCompare (>=) l r o
string ["COMPARE", mode, _, _, _] cs = liftIO $ fail "" <* cmFormattedError FatalError (Just "string") ["string sub-command COMPARE does not recognize mode ", mode] cs
string ["COMPARE"] cs = liftIO $ fail "" <* cmFormattedError FatalError (Just "string") ["string sub-command COMPARE requires a mode to be specified."] cs
string ("COMPARE" : _) cs = raiseArgumentCountError "string" cs

-- GENERATION
string ["HEX", str, outputVar] _ = alt currentScope $ setVariable outputVar (BS16.encode str)

string _ cs = raiseArgumentCountError "string" cs

-- Utility methods

findSubstring :: ByteString -> ByteString -> Bool -> Int
findSubstring sub str False = findSubstring' sub str init'
  where
    init' :: [m] -> [m]
    init' [] = []
    init' s  = init s
findSubstring sub str True = findSubstring' sub str tail'
  where
    tail' :: [m] -> [m]
    tail' [] = []
    tail' s  = tail s

findSubstring' :: ByteString -> ByteString -> ([Int] -> [Int]) -> Int
findSubstring' sub str f = fromMaybe (-1 :: Int) $ listToMaybe (f (BSS.indices sub str))

stringCompare :: (ByteString -> ByteString -> Bool) -> ByteString -> ByteString -> ByteString -> Interp ()
stringCompare op l r o = alt currentScope $ setVariable o (BS.pack . show . fromEnum $ op l r)
