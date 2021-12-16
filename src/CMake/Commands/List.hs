{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.Commands.List (list) where
import CMake.Error (raiseArgumentCountError, CmErrorKind (FatalError), cmFormattedError)
import Data.List.Extra (splitOn, intercalate, elemIndex)
import CMake.Interpreter.State
    ( CmBuiltinCommand, CmState(CmState, currentScope), setVariable, readVariable, CmScope )
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import CMake.List (splitCmList, joinCmList)
import Text.Read (readMaybe)

import qualified Data.ByteString.Base16     as BS16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Search     as BSS
import Data.ByteString (ByteString, unpack)

list :: CmBuiltinCommand

list ["LENGTH", list, outputVar] a s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack $ show $ length $ getList list currentScope) currentScope}
list ("GET" : list : rest) callsite s@CmState{currentScope}
    | length rest >= 2 = --at least first index and outputVar
        let
            listA = getList list currentScope
            listL = length listA
            indiciesLength = length rest - 1
            indicies = map (readMaybe . BS.unpack) (take indiciesLength rest)
            outputVar = last rest
        in
            if all isJust indicies && not (null listA)
                then pure $ Just s{currentScope=setVariable outputVar (joinCmList [listA !! mod (fromJust index) listL | index <- indicies]) currentScope}
                else raiseArgumentCountError (BS.pack "Invalid arguments for list Get") callsite
                    --Just s <$ cmFormattedError FatalError (Just "string") "Invalid arguments for list Get" callsite
    | otherwise = raiseArgumentCountError "list GET" callsite
list ["JOIN" , list , glue, outputVar] callsite s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.intercalate glue listItems) currentScope}
    where listItems = getList list currentScope
list ["SUBLIST" , list , begin, length, outputVar] callsite s@CmState{currentScope} =
    let
        lengthI = readMaybe $ BS.unpack length
        beginI = readMaybe $ BS.unpack begin
        justLength = fromJust lengthI
        justBegin = fromJust beginI
        listA = getList list currentScope
    in
        if isNothing lengthI || isNothing beginI
            then Just s <$ cmFormattedError FatalError (Just "list") ["Cannot SUBLIST as length or begining index can't be parsed."] callsite
            else if justLength == -1
                then pure $ Just s{currentScope=setVariable outputVar (joinCmList $ drop justBegin listA) currentScope}
                else pure $ Just s{currentScope=setVariable outputVar (joinCmList $ take justLength $ drop justBegin listA) currentScope}
list ["FIND" , list , value, outputVar] callsite s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack $ show $ fromMaybe (-1) (elemIndex value $ getList list currentScope)) currentScope}
list ("APPEND" : list : args) callsite s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable list (joinCmList $ getList list currentScope ++ args) currentScope}
list ("INSERT" : list : index : args) callsite s@CmState{currentScope} = case iRes of
    Nothing -> Just s <$ cmFormattedError FatalError (Just "list") ["Cannot INSERT as index can't be parsed."] callsite
    Just i -> pure $ Just s{currentScope=setVariable list (joinCmList $ begining ++ args ++ end) currentScope}
    where
        iRes = readMaybe $ BS.unpack index
        (begining, end) = splitAt (fromJust iRes) $ getList list currentScope
    (REMOVE_AT), (REMOVE_DUPLICATES), (REVERSE)
list ("POP_BACK" : list : args) callsite s@CmState{currentScope} =
    Just s <$ cmFormattedError FatalError (Just "list") ["Not implemented"] callsite
list ("POP_FRONT" : list : args) callsite s@CmState{currentScope} =
    Just s <$ cmFormattedError FatalError (Just "list") ["Not implemented"] callsite
list ("REMOVE_AT" : list : args) callsite s@CmState{currentScope} =
    Just s <$ cmFormattedError FatalError (Just "list") ["Not implemented"] callsite
list ("REMOVE_DUPLICATES" : list : args) callsite s@CmState{currentScope} =
    Just s <$ cmFormattedError FatalError (Just "list") ["Not implemented"] callsite
list ("REVERSE" : list : args) callsite s@CmState{currentScope} =
    Just s <$ cmFormattedError FatalError (Just "list") ["Not implemented"] callsite
list ("PREPEND" : list : args) callsite s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable list (joinCmList $ args ++ getList list currentScope ) currentScope}
list ["PREPEND", list, item] callsite s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable list (joinCmList $ filter (== item) $ getList list currentScope ) currentScope}

list args callsite _ = raiseArgumentCountError (BS.pack $ "list " ++ show args) callsite

-- NOT IMPLEMENTED: (POP_BACK), (POP_FRONT)
-- TODO (REMOVE_AT), (REMOVE_DUPLICATES), (REVERSE)


-- UTILITY
getList :: ByteString -> CmScope -> [ByteString]
getList list scope = splitCmList savedVar
    where savedVar = fromMaybe "" $ readVariable list scope

