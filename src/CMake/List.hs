-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake list handling utilities
----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
module CMake.List (joinCmList, splitCmList) where
import           Control.Applicative     ((<|>))
import           Data.Functor            (($>))
import           Data.List               (intercalate)
import           Text.Parser.Char        (char, notChar, string)
import           Text.Parser.Combinators (eof, many, sepBy)
import           Text.Trifecta.Parser    (Parser, parseString)
import           Text.Trifecta.Result    (Result (..))

joinCmList :: [String] -> String
joinCmList = intercalate ";"

splitCmList :: String -> [String]
splitCmList "" = []
splitCmList s = fromResult $ parseString anchorArgs mempty s

fromResult :: Result [String] -> [String]
fromResult = \case
              Success xs -> xs
              _          -> error "Internal parsing failure"

anchorArgs :: Parser [String]
anchorArgs = many unescapeSemi `sepBy` char ';' <* eof
unescapeSemi :: Parser Char
unescapeSemi = (string "\\;" $> ';') <|> notChar ';'
