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
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           CMake.Interpreter.State  (CmScope (..), emptyScope,
                                           readVariable, setVariable)
import           CMake.List               (joinCmList, splitCmList)
import           CMakeHs.Internal.Functor ((<$$>))
import           Control.Applicative      (many, some, (<|>))
import           Control.Monad            (liftM2)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Foldable            (foldl')
import           Data.Functor             (($>))
import           Data.Maybe               (fromMaybe)
import           System.Environment       (lookupEnv)
import           Text.Parser.Char         (notChar, string)
import           Text.Trifecta            (ErrInfo (..), Parser, Result (..),
                                           eof, parseByteString)

autoDeref :: ByteString -> CmScope -> ByteString
autoDeref name s = fromMaybe name $ readVariable name s

braced :: ByteString -> ByteString -> Maybe ByteString
braced tok arg
  | not $ BS.append tok "{" `BS.isPrefixOf` arg = Nothing
  | not $ "}" `BS.isSuffixOf` arg = Nothing
  | otherwise = pure $ BS.init $ BS.drop (BS.length tok + 1) arg

expandArguments :: Arguments -> CmScope -> IO (Maybe [ByteString])
expandArguments [] _  = pure $ Just []
expandArguments (x:xs) s = expandArgument x s >>= maybe (pure Nothing) (\c -> (c++) <$$> expandArguments xs s)

expandArgument :: Argument -> CmScope -> IO (Maybe [ByteString])
expandArgument  (str, BracketArgument) _ = pure $ Just [str]
expandArgument (str, QuotedArgument) s   = fmap (:[]) <$> expandString str s
expandArgument (str, UnquotedArgument) s = fmap splitCmList <$> expandString str s


expandString :: ByteString -> CmScope -> IO (Maybe ByteString)
expandString str s = case parseByteString (expandingArgParse s) mempty str of
    Success expanded           -> Just <$> expanded
    Failure ErrInfo{_errDoc} ->
      cmFormattedError FatalError Nothing ["Syntax error in cmake code\n", BS.pack $ show _errDoc] unknownLocation $> Nothing

expandingArgParse :: CmScope -> Parser (IO ByteString)
expandingArgParse s = mconcat <$> many (notRef <|> ref <|> dollarLit) <* eof
  where
    notRef :: Parser (IO ByteString)
    notRef = pure . BS.pack <$> some (notChar '$')
    ref = flip expandVarRef s <$> AST.variableReference
    dollarLit :: Parser (IO ByteString)
    dollarLit = pure . BS.pack <$> string "$"

expandVarRef :: VariableReference -> CmScope -> IO ByteString
expandVarRef (VariableReference l sections) s = (\n -> expandVarRef' l n s) =<< varName
  where
    varName :: IO ByteString
    varName = mconcat $ flipM expandVarRefSect (pure s) sections
    flipM :: Monad m => (a -> b -> c) -> m b -> m a -> m c
    flipM f a b = liftM2 (flip f) a b

expandVarRef' :: VariableLookup -> ByteString -> CmScope -> IO ByteString
expandVarRef' Scope name s = pure $ fromMaybe "" $ readVariable name s
expandVarRef' Cache _ _    = pure "" -- Cache does not exist in script mode
expandVarRef' Env name _   = BS.pack . fromMaybe "" <$> lookupEnv (BS.unpack name)

expandVarRefSect :: VariableReferenceSection -> CmScope -> IO ByteString
expandVarRefSect (IdentifierSection str) _ = pure str
expandVarRefSect (NestedReference nr) s    = expandVarRef nr s

applyFuncArgs :: Arguments -> [ByteString] -> CmScope
applyFuncArgs fa ia = setVariable "ARGV" (joinCmList ia)
                    $ setVariable "ARGC" (BS.pack $ show $ length ia)
                    $ setVariable "ARGN" (joinCmList $ drop (length fa) ia)
                    $ ffoldl' (uncurry setVariable) (zip (fst <$> fa) ia)
                    $ ffoldl' (\(i,v) -> setVariable (BS.append "ARGV" (BS.pack (show i))) v) (zip [(0 :: Int)..] ia) emptyScope
  where
    ffoldl' :: (a -> b -> b) -> [a] -> b -> b
    ffoldl' r = flip (foldl' (flip r))
