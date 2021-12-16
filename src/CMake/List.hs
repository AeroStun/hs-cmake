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
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module CMake.List (joinCmList, splitCmList) where
import           Control.Applicative     ((<|>))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import           Data.Functor            (($>))
import           Text.Parser.Char        (char, notChar, string)
import           Text.Parser.Combinators (eof, many, sepBy)
import           Text.Trifecta.Parser    (Parser, parseByteString)
import           Text.Trifecta.Result    (Result (..))

joinCmList :: [ByteString] -> ByteString
joinCmList = BS.intercalate ";"

splitCmList :: ByteString -> [ByteString]
splitCmList "" = []
splitCmList s  = fromResult $ parseByteString anchorArgs mempty s

fromResult :: Result [m] -> [m]
fromResult = \case
              Success xs -> xs
              _          -> error "Internal parsing failure"

anchorArgs :: Parser [ByteString]
anchorArgs =  (BS.pack <$> many unescapeSemi) `sepBy` char ';' <* eof
unescapeSemi :: Parser Char
unescapeSemi = (string "\\;" $> ';') <|> notChar ';'
