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
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Interpreter.Arguments (
  autoDeref,
  braced,
  expandArguments,
  applyFuncArgs
  ) where
import           CMake.AST.Defs           (Argument, ArgumentKind (..),
                                           Arguments, VariableLookup (..),
                                           VariableReference (..),
                                           VariableReferenceSection (..),
                                           unknownLocation)
import qualified CMake.AST.Parser         as AST (variableReference)
import           CMake.Error              (CmErrorKind (..), cmFormattedError)
import           CMake.Interpreter.State  (CmScope (..), readVariable,
                                           setVariable)
import           CMake.List               (joinCmList, splitCmList)
import           CMakeHs.Internal.Functor ((<$$>))
import           Control.Applicative      (many, some, (<|>))
import           Control.Monad            (liftM2)
import           Data.Foldable            (foldl')
import           Data.Functor             (($>))
import           Data.List                (isPrefixOf, isSuffixOf)
import           Data.Maybe               (fromMaybe)
import           System.Environment       (lookupEnv)
import           Text.Parser.Char         (notChar, string)
import           Text.Trifecta            (ErrInfo (..), Parser, Result (..),
                                           eof, parseString)

autoDeref :: String -> CmScope -> String
autoDeref name s = fromMaybe name $ readVariable name s

braced :: String -> String -> Maybe String
braced tok arg
  | not $ (tok ++ "{") `isPrefixOf` arg = Nothing
  | not $ "}" `isSuffixOf` arg = Nothing
  | otherwise = pure $ init $ drop (length tok + 1) arg

expandArguments :: Arguments -> CmScope -> IO (Maybe [String])
expandArguments [] _  = pure $ Just []
expandArguments (x:xs) s = expandArgument x s >>= maybe (pure Nothing) (\c -> (c++) <$$> expandArguments xs s)

expandArgument :: Argument -> CmScope -> IO (Maybe [String])
expandArgument  (str, BracketArgument) _ = pure $ Just [str]
expandArgument (str, QuotedArgument) s   = fmap (:[]) <$> expandString str s
expandArgument (str, UnquotedArgument) s = fmap splitCmList <$> expandString str s


expandString :: String -> CmScope -> IO (Maybe String)
expandString str s = case parseString (expandingArgParse s) mempty str of
    Success expanded           -> Just <$> expanded
    Failure ErrInfo{_errDoc} ->
      cmFormattedError FatalError Nothing ("Syntax error in cmake code\n" ++ show _errDoc) unknownLocation $> Nothing

expandingArgParse :: CmScope -> Parser (IO String)
expandingArgParse s = mconcat <$> many (notRef <|> ref <|> dollarLit) <* eof
  where
    notRef :: Parser (IO String)
    notRef = pure <$> some (notChar '$')
    ref = flip expandVarRef s <$> AST.variableReference
    dollarLit :: Parser (IO String)
    dollarLit = pure <$> string "$"

expandVarRef :: VariableReference -> CmScope -> IO String
expandVarRef (VariableReference l sections) s = (\n -> expandVarRef' l n s) =<< varName
  where
    varName :: IO String
    varName = mconcat $ flipM expandVarRefSect (pure s) sections
    flipM :: Monad m => (a -> b -> c) -> m b -> m a -> m c
    flipM f a b = liftM2 (flip f) a b

expandVarRef' :: VariableLookup -> String -> CmScope -> IO String
expandVarRef' Scope name s = pure $ fromMaybe "" $ readVariable name s
expandVarRef' Cache _ _    = pure "" -- Cache does not exist in script mode
expandVarRef' Env name _   = fromMaybe "" <$> lookupEnv name

expandVarRefSect :: VariableReferenceSection -> CmScope -> IO String
expandVarRefSect (IdentifierSection str) _ = pure str
expandVarRefSect (NestedReference nr) s    = expandVarRef nr s

applyFuncArgs :: Arguments -> Arguments -> CmScope -> CmScope
applyFuncArgs fa ia s = setVariable "ARGV" (joinCmList $ fst <$> ia)
                      $ setVariable "ARGC" (show $ length ia)
                      $ setVariable "ARGN" (joinCmList $ drop (length fa) (fst <$> ia))
                      $ ffoldl' (uncurry setVariable) (zip (fst <$> fa) (fst <$> ia))
                      $ ffoldl' (\(i,v) -> setVariable ("ARGV" ++ show i) v) (zip [(0 :: Int)..] (fst <$> ia)) s
  where
    ffoldl' :: (a -> b -> b) -> [a] -> b -> b
    ffoldl' r = flip (foldl' (flip r))
