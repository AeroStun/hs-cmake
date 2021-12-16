{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.Commands.List (list) where
import           CMake.Error              (CmErrorKind (FatalError),
                                           cmFormattedError,
                                           raiseArgumentCountError)
import           CMake.Interpreter.State  (CmBuiltinCommand, CmScope,
                                           CmState (CmState, currentScope),
                                           readVariable, setVariable)
import           CMake.List               (joinCmList, splitCmList)
import           Data.List.Extra          (elemIndex)
import           Data.Maybe               (fromJust, fromMaybe, isJust,
                                           isNothing)
import           Text.Read                (readMaybe)

import           CMakeHs.Internal.Numeric (readMaybeInt)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS

list :: CmBuiltinCommand

list ["LENGTH", l, outputVar] _ s@CmState{currentScope=ps} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack . show . length $ getList l ps) ps}
list ["GET", l, iS, o] cs s@CmState{currentScope=ps}
  | Just i <- readMaybeInt iS = pure $ Just s{currentScope=setVariable o (getList l ps !! i) ps}
  | otherwise = Nothing <$ cmFormattedError FatalError (Just "list") ["index for GET is not an integer."] cs
list ("GET" : l : rest) cs s@CmState{currentScope}
    | length rest >= 2 = --at least first index and outputVar
        let
            listA = getList l currentScope
            listL = length listA
            indices = map (readMaybe . BS.unpack) (init rest)
            outputVar = last rest
        in
            if all isJust indices && not (null listA)
                then pure $ Just s{currentScope=setVariable outputVar (joinCmList [listA !! mod (fromJust index) listL | index <- indices]) currentScope}
                else raiseArgumentCountError "list" cs
    | otherwise = raiseArgumentCountError "list GET" cs
list ["JOIN" , l , glue, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.intercalate glue listItems) currentScope}
    where listItems = getList l currentScope
list ["SUBLIST", l, beginS, lenS, o] cs s@CmState{currentScope} =
    let
        lengthI = readMaybe $ BS.unpack lenS
        beginI = readMaybe $ BS.unpack beginS
        justLength = fromJust lengthI
        justBegin = fromJust beginI
        listA = getList l currentScope
    in
        if isNothing lengthI || isNothing beginI
            then Just s <$ cmFormattedError FatalError (Just "list") ["Cannot SUBLIST as length or begining index can't be parsed."] cs
            else if justLength == -1
                then pure $ Just s{currentScope=setVariable o (joinCmList $ drop justBegin listA) currentScope}
                else pure $ Just s{currentScope=setVariable o (joinCmList $ take justLength $ drop justBegin listA) currentScope}
list ["FIND", l, value, outputVar] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable outputVar (BS.pack $ show $ fromMaybe (-1) (elemIndex value $ getList l currentScope)) currentScope}
list ("APPEND" : l : args) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable l (joinCmList $ getList l currentScope ++ args) currentScope}
list ("INSERT" : l : index : args) cs s@CmState{currentScope} = case iRes of
    Nothing -> Just s <$ cmFormattedError FatalError (Just "list") ["Cannot INSERT as index cannot be parsed."] cs
    Just _ -> pure $ Just s{currentScope=setVariable l (joinCmList $ beginning ++ args ++ end) currentScope}
    where
        iRes = readMaybe $ BS.unpack index
        (beginning, end) = splitAt (fromJust iRes) $ getList l currentScope
list ("POP_BACK" : _ : _) cs _ =
    Nothing <$ cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("POP_FRONT" : _ : _) cs _ =
    Nothing <$ cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REMOVE_AT" : _ : _) cs _ =
    Nothing <$ cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REMOVE_DUPLICATES" : _ : _) cs _ =
    Nothing <$ cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REVERSE" : _ : _) cs _ =
    Nothing <$ cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("PREPEND" : l : args) _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable l (joinCmList $ args ++ getList l currentScope ) currentScope}
list ["PREPEND", l, item] _ s@CmState{currentScope} =
    pure $ Just s{currentScope=setVariable l (joinCmList $ filter (== item) $ getList l currentScope ) currentScope}

list _ cs _ = raiseArgumentCountError "list" cs

-- NOT IMPLEMENTED: (POP_BACK), (POP_FRONT)
-- TODO (REMOVE_AT), (REMOVE_DUPLICATES), (REVERSE)


-- UTILITY
getList :: ByteString -> CmScope -> [ByteString]
getList l s = splitCmList $ fromMaybe "" $ readVariable l s
