-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `math` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module CMake.Commands.Math (math) where

import           CMake.AST.Defs              (SourceLocation)
import           CMake.Error                 (raiseArgumentCountError,
                                              raiseFatalError)
import           CMake.Interpreter.ArgsParse (ArgsParser, parseArgs, unaryArg,
                                              unaryOpt)
import           CMake.Interpreter.State     (CmBuiltinCommand, Interp, alt,
                                              currentScope, setVariable)
import           Control.Applicative         (many, (<|>))
import           Data.Bits                   (complement, shiftL, shiftR, xor,
                                              (.&.), (.|.))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                (($>))
import           Data.Int                    (Int64)
import           Data.Label                  (mkLabel)
import           Numeric.Extra               (readDec, readHex, showHex)
import           ParserT                     (ParserT (..), chainl1, item,
                                              parseList, tok, toks)
import           Text.Parser.Combinators     (between)

data Base = Decimal | Hexadecimal deriving (Eq, Show)

data MathArgs = MathArgs
                { _var     :: ByteString
                , _exprStr ::  ByteString
                , _base    :: Base
                } deriving Show
mkLabel ''MathArgs

math :: CmBuiltinCommand
math ("EXPR" : xs) cs = parseArgs "math(EXPR)" xs parser baseArgs cs >>= flip math' cs
  where
    parser :: ArgsParser ByteString MathArgs ()
    parser = unaryArg "variable" var (const item)
          >> unaryArg "expression" exprStr (const item)
          >> unaryOpt "OUTPUT_FORMAT" base (const $ tok "HEXADECIMAL" $> Hexadecimal <|> tok "DECIMAL" $> Decimal)
    baseArgs = MathArgs mempty mempty Decimal
math _ cs = raiseArgumentCountError "math" cs

showBase :: Base -> Int64 -> ByteString
showBase Decimal     v = BS.pack $ show v
showBase Hexadecimal v = BS.pack $ "0x" ++ showHex v ""

math' :: MathArgs -> SourceLocation -> Interp ()
math' MathArgs{..} cs = math'' _exprStr cs >>= \v -> alt currentScope (setVariable _var (showBase _base v))

math'' :: ByteString -> SourceLocation -> Interp Int64
math'' s cs = case eval s of
                Just v -> pure v
                Nothing -> raiseFatalError "math" ["math cannot parse the expression"] cs

eval :: ByteString -> Maybe Int64
eval s = parseList (many (tok ' ') *> expr) (BS.unpack s)

type ExprParser r = ParserT Char r
type ExprReducer = ExprParser Int64

token :: ExprParser a -> ExprParser a
token p = p <* many (tok ' ')

number :: ExprReducer
number = ((toks "0x" *> ParserT readHex) <|> ParserT readDec) <* many (tok ' ')


expr :: ExprReducer
expr = term

-- Terms are defined in from lowest to highest precedence,
-- with a ' for every level

term :: ExprReducer
term = term' `chainl1` infixOp "|" (.|.)

term' :: ExprReducer
term' = term'' `chainl1` infixOp "^" xor

term'' :: ExprReducer
term'' = term''' `chainl1` infixOp "&" (.&.)

term''' :: ExprReducer
term''' = term'''' `chainl1` shOp

term'''' :: ExprReducer
term'''' = term''''' `chainl1` addOp

term''''' :: ExprReducer
term''''' = term'''''' `chainl1` mulOp

term'''''' :: ExprReducer
term'''''' = number <|> parenthesized <|> prefixOp "-" negate <|> notOp <|> prefixOp "+" id

parenthesized :: ExprReducer
parenthesized = between (token $ tok '(') (token $ tok ')') expr

infixOp :: String -> (Int64 -> Int64 -> Int64) -> ExprParser (Int64 -> Int64 -> Int64)
infixOp x f = token (toks x) >> return f

prefixOp :: String -> (Int64 -> Int64) -> ExprReducer
prefixOp x f = token (toks x) *> (f <$> term'''''')

addOp :: ExprParser (Int64 -> Int64 -> Int64)
addOp = infixOp "+" (+) <|> infixOp "-" (-)

mulOp :: ExprParser (Int64 -> Int64 -> Int64)
mulOp = infixOp "*" (*) <|> infixOp "/" quot <|> infixOp "%" rem

shOp :: ExprParser (Int64 -> Int64 -> Int64)
shOp = infixOp ">>" (\l r -> l `shiftR` fromIntegral r)
   <|> infixOp "<<" (\l r -> l `shiftL` fromIntegral r)

notOp :: ExprReducer
notOp = tok '~' >> complement <$> number
