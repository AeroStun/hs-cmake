{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CMake.Commands.String where
import           CMake.Error             (CmErrorKind (..), cmFormattedError,
                                          raiseArgumentCountError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmScope (..),
                                          CmState (..), setVariable, readVariable)
import Data.Maybe (fromMaybe)
import Data.List (findIndices, elemIndices, isPrefixOf, intercalate)
import Data.Char (toUpper, toLower)

string :: CmBuiltinCommand


string [] callsite _ = raiseArgumentCountError "string" callsite
-- SEARCH AND REPLACE
string ["FIND", string, substring, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (show $ findSubstring substring string False) currentScope}
string ["FIND", string, substring, outputVar, "REVERSE"] _ s@CmState{currentScope} = 
    pure $ Just s{currentScope=setVariable outputVar (show $ findSubstring substring string True) currentScope}
string ("FIND": _) callSite _ = raiseArgumentCountError "FIND WRONG ARGUMENTS" callSite
--string ("REPLACE" : mtchString : replString : outputVar : input : inputs)

-- MANIPULATION
string ("APPEND": stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (savedVar ++ concat inputs) currentScope}
    where savedVar = fromMaybe "" $ readVariable stringVar currentScope
string ("CONCAT": stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (concat inputs) currentScope}
string ("JOIN": glue : stringVar: inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (intercalate glue inputs) currentScope}
string ("PREPEND" : stringVar : inputs) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (concat inputs ++ savedVar) currentScope}
    where savedVar = fromMaybe "" $ readVariable stringVar currentScope
string ["LENGTH", string, output] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable output (show $ length string) currentScope}
string ["SUBSTRING", string, begin, length, outputVar] _ s@CmState{currentScope} = if lengthI == -1
    then pure $ Just s{currentScope=setVariable outputVar (drop beginI string) currentScope}
    else pure $ Just s{currentScope=setVariable outputVar (take lengthI $ drop beginI string) currentScope}
    where 
        lengthI = read length
        beginI = read begin
string ["TOLOWER", string, stringVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (map toLower string) currentScope}
string ["TOUPPER", string, stringVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable stringVar (map toUpper string) currentScope}
string _ callsite _ = raiseArgumentCountError "set" callsite


--Utility methods



findSubstring :: String -> String -> Bool -> Int
findSubstring [] _ _ = -1 --Default when substring is empty, NOT SURE IF THIS IS CORRECT 
findSubstring sub@(x:xs) string reverseOrder
    | not reverseOrder = findSubstring' (elemIndices x string) sub  string
    | reverseOrder     = findSubstring' (reverse (elemIndices x string)) sub  string
findSubstring' :: [Int] -> String -> String -> Int
findSubstring' [] _ _ = -1 --Default when substring is empty, NOT SURE IF THIS IS CORRECT 
findSubstring' (i:is) sub string = if isPrefixOf sub $ drop i string
    then i
    else findSubstring' is sub string

