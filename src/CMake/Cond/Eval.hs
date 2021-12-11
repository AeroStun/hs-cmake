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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module CMake.Cond.Eval (evalCond, DefinedLookup(..)) where
import           CMake.Cond.Defs             (BinaryOp (..), Cond (..),
                                              UnaryOp (..), truthy)
import           CMake.Interpreter.Arguments (autoDeref)
import           CMake.Interpreter.State     (CmScope (..), CmState (..),
                                              hasVariable, readVariable)
import           CMake.List                  (splitCmList)
import           Control.Applicative         (Alternative, many, some, (<|>))
import           Control.Exception           (IOException, catch)
import           Control.Monad               (liftM2)
import qualified Data.CaseInsensitive        as CI (mk)
import           Data.Functor                (($>))
import           Data.HashMap.Strict         (member)
import           Data.Maybe                  (fromMaybe, isJust)
import           System.Directory            (doesDirectoryExist, doesPathExist,
                                              pathIsSymbolicLink)
import           System.Environment          (lookupEnv)
import           System.FilePath             (isAbsolute)
import           Text.Parser.Combinators     (manyTill, try)
import           Text.Read                   (readMaybe)
import           Text.Trifecta               (Parser, Result (..), anyChar,
                                              char, eof, parseString, string)

evalCond :: Cond -> CmState -> IO Bool
evalCond (Parenthesized cond)  s = evalCond cond s
evalCond (Or lhs rhs)          s = evalCond lhs s >>= (\c -> if c then return True else evalCond rhs s)
evalCond (And lhs rhs)         s = evalCond lhs s >>= (\c -> if c then evalCond rhs s else return False)
evalCond (Not cond)            s = not <$> evalCond cond s
evalCond (BinaryOp lhs op rhs) s = evalBinary op lhs rhs (currentScope s)
evalCond (UnaryOp op arg)      s = evalUnary op arg s
evalCond (Constant b)          _ = pure b
evalCond (VariableOrString n)  CmState{currentScope} = pure $ maybe True truthy (readVariable n currentScope)

evalUnary :: UnaryOp -> String -> CmState -> IO Bool
evalUnary Command     a CmState{commands}     = pure $ member (CI.mk a) commands
evalUnary Policy      _ _                     = putStrLn "Unsupported check POLICY" $> False
evalUnary Target      _ _                     = pure False -- Targets do not exist in script mode
evalUnary Test        _ _                     = pure False -- Tests do not exist in script mode
evalUnary Defined     a CmState{currentScope} =
    case parseString definedLookup mempty a of
      Success (Scope, n) -> pure $ hasVariable n currentScope
      Success (Env, n)   -> isJust <$> lookupEnv n
      Success (Cache, _) -> pure False -- Cache does not exist in script mode
      _                  -> error "Internal parsing failure" -- may not fail
evalUnary Exists      a _                     = doesPathExist a
evalUnary IsDirectory a _                     = doesDirectoryExist a
evalUnary IsSymlink   a _                     = catch (pathIsSymbolicLink a) ((const $ pure False) :: IOException -> IO Bool)
evalUnary IsAbsolute  a _                     = pure $ isAbsolute a

data DefinedLookup = Scope | Cache | Env deriving (Eq, Show)
definedLookup :: Parser (DefinedLookup, String)
definedLookup = (Env,)   <$> try (string "ENV{" *> someTill anyChar (char '}' <* eof))
           <|> (Cache,) <$> try (string "CACHE{" *> someTill anyChar (char '}' <* eof))
           <|> (Scope,) <$> many anyChar
    where
      someTill :: (Monad m, Alternative m) => m a -> m b -> m [a]
      someTill p e = liftM2 (:) p (manyTill p e)


evalBinary :: BinaryOp -> String -> String -> CmScope -> IO Bool
evalBinary InList              l r s = pure $ maybe False (\ls -> autoDeref l s `elem` splitCmList ls) (readVariable r s)
evalBinary IsNewerThan         l r s = undefined
evalBinary Matches             _ _ _ = putStrLn "Unsupported check MATCHES" $> False
evalBinary Less                l r s = binaryAutoDeref (<) readMaybeInt l r s
evalBinary Greater             l r s = binaryAutoDeref (>) readMaybeInt l r s
evalBinary LessEqual           l r s = binaryAutoDeref (<=) readMaybeInt l r s
evalBinary GreaterEqual        l r s = binaryAutoDeref (>=) readMaybeInt l r s
evalBinary StrLess             l r s = binaryAutoDeref (<) Just l r s
evalBinary StrGreater          l r s = binaryAutoDeref (>) Just l r s
evalBinary StrEqual            l r s = binaryAutoDeref (==) Just l r s
evalBinary StrLessEqual        l r s = binaryAutoDeref (<=) Just l r s
evalBinary StrGreaterEqual     l r s = binaryAutoDeref (>=) Just l r s
evalBinary VersionLess         _ _ _ = putStrLn "Unsupported check VERSION_LESS" $> False
evalBinary VersionGreater      _ _ _ = putStrLn "Unsupported check VERSION_GREATER" $> False
evalBinary VersionEqual        _ _ _ = putStrLn "Unsupported check VERSION_EQUAL" $> False
evalBinary VersionLessEqual    _ _ _ = putStrLn "Unsupported check VERSION_LESS_EQUAL" $> False
evalBinary VersionGreaterEqual _ _ _ = putStrLn "Unsupported check VERSION_GREATER_EQUAL" $> False

binaryAutoDeref :: Monad m => (a -> a -> Bool) -> (String -> Maybe a) -> String -> String -> CmScope -> m Bool
binaryAutoDeref o m l r s = return $ fromMaybe False $ liftM2 o (m (autoDeref l s)) (m (autoDeref r s))

readMaybeInt = readMaybe :: String -> Maybe Int

