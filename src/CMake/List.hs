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
module CMake.List (splitCmList) where
import           Control.Applicative     ((<|>))
import           Data.Functor            (($>))
import           Text.Parser.Char        (char, notChar, string)
import           Text.Parser.Combinators (eof, many, sepBy)
import           Text.Trifecta.Parser    (Parser, parseString)
import           Text.Trifecta.Result    (Result (..))


splitCmList :: String -> [String]
splitCmList = fromResult . parseString anchorArgs mempty

fromResult :: Result [String] -> [String]
fromResult = \case
              Success xs -> xs
              _          -> error "Internal parsing failure"

anchorArgs :: Parser [String]
anchorArgs = many unescapeSemi `sepBy` char ';' <* eof
unescapeSemi :: Parser Char
unescapeSemi = (string "\\;" $> ';') <|> notChar ';'
