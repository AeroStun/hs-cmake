{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.Commands.String (string) where
import           CMake.Error                (CmErrorKind (..), cmFormattedError,
                                             raiseArgumentCountError)
import           CMake.Interpreter.State    (CmBuiltinCommand, CmState (..),
                                             readVariable, setVariable)
import           CMakeHs.Internal.Numeric   (readMaybeInt)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as BS16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Search     as BSS
import           Data.Char                  (isSpace, toLower, toUpper)
import           Data.Maybe                 (fromMaybe, listToMaybe)

string :: CmBuiltinCommand
string [] callsite _ = raiseArgumentCountError "string" callsite

-- SEARCH AND REPLACE
string ["FIND", haystack, needle, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack $ show $ findSubstring needle haystack False) currentScope}
string ["FIND", haystack, needle, outputVar, "REVERSE"] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack $ show $ findSubstring needle haystack True) currentScope}
string ("FIND": _) callSite _ = raiseArgumentCountError "string" callSite
string ("REPLACE" : m : r : o : x : xs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable o (LBS.toStrict $ BSS.replace m r $ BS.concat (x : xs)) currentScope}

-- MANIPULATION
string ("APPEND": stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.concat (savedVar : inputs)) currentScope}
  where savedVar = fromMaybe "" $ readVariable stringVar currentScope
string ("PREPEND" : stringVar : inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.concat (inputs ++ [savedVar])) currentScope}
  where savedVar = fromMaybe "" $ readVariable stringVar currentScope
string ("CONCAT": stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.concat inputs) currentScope}
string ("JOIN": glue : stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.intercalate glue inputs) currentScope}
string ["TOLOWER", str, stringVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.map toLower str) currentScope}
string ["TOUPPER", str, stringVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (BS.map toUpper str) currentScope}
string ["LENGTH", str, output] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable output (BS.pack $ show $ BS.length str) currentScope}
string ["SUBSTRING", str, beginS, lenS, outputVar] cs s@CmState{currentScope} =
    case (readMaybeInt beginS, readMaybeInt lenS) of
      (Just begin, _)
        | begin < 0 || begin >= BS.length str -> Nothing <$ cmFormattedError FatalError (Just "string") ["string begin index is out of range"] cs
      (Just begin, Just (-1)) -> pure $ Just s{currentScope=setVariable outputVar (BS.drop begin str) currentScope}
      (Just begin, Just len) -> pure $ Just s{currentScope=setVariable outputVar (BS.take len $ BS.drop begin str) currentScope}
      _ -> raiseArgumentCountError "string" cs
string ["STRIP", str, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (fst $ BS.breakEnd isSpace $ snd $ BS.span isSpace str) currentScope}
string ["REPEAT", str, countS, outputVar] cs s@CmState{currentScope} =
    case readMaybeInt countS of
      Just count
        | count >= 0 -> pure $ Just s{currentScope=setVariable outputVar (BS.concat $ replicate count str) currentScope}
      _ -> Nothing <$ cmFormattedError FatalError (Just "string") ["repeat count is not a positive number."] cs


-- COMPARISON
string ["COMPARE", "LESS",          l, r, o] _ s = pure . Just $ stringCompare (<)  l r o s
string ["COMPARE", "GREATER",       l, r, o] _ s = pure . Just $ stringCompare (>)  l r o s
string ["COMPARE", "EQUAL",         l, r, o] _ s = pure . Just $ stringCompare (==) l r o s
string ["COMPARE", "NOTEQUAL",      l, r, o] _ s = pure . Just $ stringCompare (/=) l r o s
string ["COMPARE", "LESS_EQUAL",    l, r, o] _ s = pure . Just $ stringCompare (<=) l r o s
string ["COMPARE", "GREATER_EQUAL", l, r, o] _ s = pure . Just $ stringCompare (>=) l r o s
string ["COMPARE", mode, _, _, _] cs _ = Nothing <$ cmFormattedError FatalError (Just "string") ["string sub-command COMPARE does not recognize mode ", mode] cs
string ["COMPARE"] cs _ = Nothing <$ cmFormattedError FatalError (Just "string") ["string sub-command COMPARE requires a mode to be specified."] cs
string ("COMPARE" : _) cs _ = raiseArgumentCountError "string" cs

-- GENERATION
string ["HEX", str, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS16.encode str) currentScope}

string _ cs _ = raiseArgumentCountError "string" cs

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

stringCompare :: (ByteString -> ByteString -> Bool) -> ByteString -> ByteString -> ByteString -> CmState -> CmState
stringCompare op l r o s@CmState{currentScope=ps} = s{currentScope=setVariable o (BS.pack . show . fromEnum $ op l r) ps}
