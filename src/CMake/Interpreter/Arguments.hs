-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Argument processing functions
----------------------------------------------------------------------------
module CMake.Interpreter.Arguments (
  expandArguments
  ) where
import           CMake.AST.Defs          (Argument (..), Arguments,
                                          VariableReference (..),
                                          VariableReferenceSection (..))
import qualified CMake.AST.Parser        as AST (arguments, variableReference)
import           CMake.Interpreter.State (CmScope (..), readVariable)
import           Control.Applicative     (some, (<|>))
import           Control.Monad           (void)
import           Data.Either.Combinators (leftToMaybe)
import           Data.Functor            (($>))
import           Data.Maybe              (fromMaybe)
import           Text.Trifecta           (ErrInfo (..), Parser, Result (..),
                                          anyChar, char, eof, foldResult, many,
                                          notChar, parseString, semi, sepBy,
                                          sepEndBy, string, try)

expandArguments :: Arguments -> CmScope -> Maybe [String]
expandArguments [] _  = Just []
expandArguments (x:xs) s = (expandArgument x s :: Maybe [String]) >>= (\c -> (c++) <$> expandArguments xs s)

expandArgument :: Argument -> CmScope -> Maybe [String]
expandArgument  (BracketArgument str) _ = Just [str]
expandArgument (QuotedArgument str) s = (:[]) <$> expandString str s
expandArgument (UnquotedArgument str) s = expandString str s >>= (resultToMaybe . parseString anchorArgs mempty)
  where
    resultToMaybe :: Result [String] -> Maybe [String]
    resultToMaybe = foldResult (const Nothing) Just
    anchorArgs :: Parser [String]
    anchorArgs = many unescapeSemi `sepEndBy` char ';' <* eof
    unescapeSemi :: Parser Char
    unescapeSemi = (string "\\;" $> ';') <|> notChar ';'

expandString :: String -> CmScope -> Maybe String
expandString str s = case parseString (expandingArgParse s) mempty str of
    Success expanded           -> Just expanded
    Failure ErrInfo{_errDoc=_} -> Nothing  -- TODO pprint failure

expandingArgParse :: CmScope -> Parser String
expandingArgParse s = concat <$> many (some (notChar '$') <|> flip expandVarRef s <$> AST.variableReference <|> (:[]) <$> char '$') <* eof
expandVarRef :: VariableReference -> CmScope -> String
expandVarRef (VariableReference sections) s = fromMaybe mempty $ readVariable (concat $ flip expandVarRefSect s <$> sections) s
expandVarRefSect :: VariableReferenceSection -> CmScope -> String
expandVarRefSect (IdentifierSection str) _ = str
expandVarRefSect (NestedReference nr) s    = expandVarRef nr s
