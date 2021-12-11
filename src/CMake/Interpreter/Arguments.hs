-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Arguments processing functions
----------------------------------------------------------------------------
module CMake.Interpreter.Arguments (
  autoDeref,
  expandArguments
  ) where
import           CMake.AST.Defs          (Argument, ArgumentKind (..),
                                          Arguments, VariableReference (..),
                                          VariableReferenceSection (..))
import qualified CMake.AST.Parser        as AST (variableReference)
import           CMake.Interpreter.State (CmScope (..), readVariable)
import           CMake.List              (splitCmList)
import           Control.Applicative     (some, (<|>))
import           Data.Maybe              (fromMaybe)
import           Text.Trifecta           (ErrInfo (..), Parser, Result (..),
                                          char, eof, many, notChar, parseString)

autoDeref :: String -> CmScope -> String
autoDeref name s = fromMaybe name $ readVariable name s

expandArguments :: Arguments -> CmScope -> Maybe [String]
expandArguments [] _  = Just []
expandArguments (x:xs) s = (expandArgument x s :: Maybe [String]) >>= (\c -> (c++) <$> expandArguments xs s)

expandArgument :: Argument -> CmScope -> Maybe [String]
expandArgument  (str, BracketArgument) _ = Just [str]
expandArgument (str, QuotedArgument) s   = (:[]) <$> expandString str s
expandArgument (str, UnquotedArgument) s = splitCmList <$> expandString str s


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
