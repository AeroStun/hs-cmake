-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Conditional tree evaluation
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CMake.Cond.Eval (evalCond) where
import           CMake.AST.Defs              (VariableLookup (..))
import           CMake.Cond.Defs             (BinaryOp (..), Cond (..),
                                              UnaryOp (..), truthy)
import           CMake.Interpreter.Arguments (autoDeref)
import           CMake.Interpreter.State     (Interp, commands, currentScope,
                                              hasVariable, readVariable, sel)
import           CMake.List                  (splitCmList)
import           Control.Applicative         (Alternative, many, (<|>))
import           Control.Exception           (IOException, catch)
import           Control.Monad               (liftM2)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI (mk)
import           Data.Functor                (($>), (<&>))
import           Data.HashMap.Strict         (member)
import           Data.Maybe                  (fromMaybe, isJust)
import           System.Directory            (doesDirectoryExist, doesPathExist,
                                              getModificationTime,
                                              pathIsSymbolicLink)
import           System.Environment          (lookupEnv)
import           System.FilePath             (isAbsolute)
import           Text.Parser.Combinators     (manyTill, try)
import           Text.Trifecta               (Parser, Result (..), anyChar,
                                              char, eof, parseByteString,
                                              string)


evalCond :: Cond -> Interp Bool
evalCond (Parenthesized cond)  = evalCond cond
evalCond (Or lhs rhs)          = evalCond lhs >>= (\c -> if c then return True else evalCond rhs)
evalCond (And lhs rhs)         = evalCond lhs >>= (\c -> if c then evalCond rhs else return False)
evalCond (Not cond)            = not <$> evalCond cond
evalCond (BinaryOp lhs op rhs) = evalBinary op lhs rhs
evalCond (UnaryOp op arg)      = evalUnary op arg
evalCond (Constant b)          = pure b
evalCond (VariableOrString n)  = maybe True truthy . readVariable n <$> sel currentScope

evalUnary :: UnaryOp -> ByteString -> Interp Bool
evalUnary Command     a = member (CI.mk a) <$> sel commands
evalUnary Policy      _ = liftIO $ putStrLn "Unsupported check POLICY" $> False
evalUnary Target      _ = pure False -- Targets do not exist in script mode
evalUnary Test        _ = pure False -- Tests do not exist in script mode
evalUnary Defined     a  =
    case parseByteString definedLookup mempty a of
      Success (Scope, n) -> hasVariable n <$> sel currentScope
      Success (Env, n)   -> liftIO $ isJust <$> lookupEnv (BS.unpack n)
      Success (Cache, _) -> pure False -- Cache does not exist in script mode
      _                  -> error "Internal parsing failure" -- may not fail
evalUnary Exists      a                      = liftIO $ doesPathExist (BS.unpack a)
evalUnary IsDirectory a                      = liftIO $ doesDirectoryExist $ BS.unpack a
evalUnary IsSymlink   a                      = liftIO $ catch (pathIsSymbolicLink $ BS.unpack a) ((const $ pure False) :: IOException -> IO Bool)
evalUnary IsAbsolute  a                      = pure $ isAbsolute $ BS.unpack a


definedLookup :: Parser (VariableLookup, ByteString)
definedLookup = (Env,) . BS.pack  <$> try (string "ENV{" *> someTill anyChar (char '}' <* eof))
           <|> (Cache,) . BS.pack <$> try (string "CACHE{" *> someTill anyChar (char '}' <* eof))
           <|> (Scope,) . BS.pack <$> many anyChar
    where
      someTill :: (Monad m, Alternative m) => m a -> m b -> m [a]
      someTill p e = liftM2 (:) p (manyTill p e)


evalBinary :: BinaryOp -> ByteString -> ByteString -> Interp Bool
evalBinary InList              l r = sel currentScope <&> (\s -> maybe False (\ls -> autoDeref l s `elem` splitCmList ls) (readVariable r s))
evalBinary IsNewerThan         l r = liftIO $ catch (liftM2 (>=) (getModificationTime $ BS.unpack l) (getModificationTime $ BS.unpack r)) ((const $ pure True) :: IOException -> IO Bool)
evalBinary Matches             _ _ = liftIO $ putStrLn "Unsupported check MATCHES" $> False
evalBinary Less                l r = binaryAutoDeref (<) readMaybeInt l r
evalBinary Greater             l r = binaryAutoDeref (>) readMaybeInt l r
evalBinary Equal               l r = binaryAutoDeref (==) readMaybeInt l r
evalBinary LessEqual           l r = binaryAutoDeref (<=) readMaybeInt l r
evalBinary GreaterEqual        l r = binaryAutoDeref (>=) readMaybeInt l r
evalBinary StrLess             l r = binaryAutoDeref (<) Just l r
evalBinary StrGreater          l r = binaryAutoDeref (>) Just l r
evalBinary StrEqual            l r = binaryAutoDeref (==) Just l r
evalBinary StrLessEqual        l r = binaryAutoDeref (<=) Just l r
evalBinary StrGreaterEqual     l r = binaryAutoDeref (>=) Just l r
evalBinary VersionLess         _ _ = liftIO $ putStrLn "Unsupported check VERSION_LESS" $> False
evalBinary VersionGreater      _ _ = liftIO $ putStrLn "Unsupported check VERSION_GREATER" $> False
evalBinary VersionEqual        _ _ = liftIO $ putStrLn "Unsupported check VERSION_EQUAL" $> False
evalBinary VersionLessEqual    _ _ = liftIO $ putStrLn "Unsupported check VERSION_LESS_EQUAL" $> False
evalBinary VersionGreaterEqual _ _ = liftIO $ putStrLn "Unsupported check VERSION_GREATER_EQUAL" $> False

binaryAutoDeref :: (a -> a -> Bool) -> (ByteString -> Maybe a) -> ByteString -> ByteString -> Interp Bool
binaryAutoDeref o m l r = sel currentScope <&> (\s -> fromMaybe False $ liftM2 o (m (autoDeref l s)) (m (autoDeref r s)))

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt bs = case BS.readInt bs of Just (v, "") -> Just v; _ -> Nothing

