-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `list` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module CMake.Commands.List (list) where
import           CMake.Error              (CmErrorKind (FatalError),
                                           cmFormattedError,
                                           raiseArgumentCountError)
import           CMake.Interpreter.State  (CmBuiltinCommand, Interp, alt,
                                           altVariable, currentScope,
                                           readVariable, sel, setVariable)
import           CMake.List               (joinCmList, splitCmList)
import           CMakeHs.Internal.Numeric (readMaybeInt)
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.List.Extra          (elemIndex)
import           Data.Maybe               (fromJust, fromMaybe, isNothing)
import           Text.Read                (readMaybe)

list :: CmBuiltinCommand

list ["LENGTH", l, o] _ = getList l >>= (\ls -> alt currentScope $ setVariable o (BS.pack . show . length $ ls))
list ["GET", l, iS, o] cs
  | Just i <- readMaybeInt iS = getList l >>= (\ls -> alt currentScope $ setVariable o (ls !! i))
  | otherwise = liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["index for GET is not an integer."] cs
list ("GET" : l : rest) cs = liftIO $ fail "Unimplemented"
list ["JOIN" , l , glue, o] _ = alt currentScope $ altVariable o (BS.intercalate glue . splitCmList)

list ["SUBLIST", l, beginS, lenS, o] cs = do
    listA <- getList l;
    let
        lengthI = readMaybe $ BS.unpack lenS
        beginI = readMaybe $ BS.unpack beginS
        justLength = fromJust lengthI
        justBegin = fromJust beginI
      in
        if isNothing lengthI || isNothing beginI
            then liftIO $ () <$ cmFormattedError FatalError (Just "list") ["Cannot SUBLIST as length or beginning index can't be parsed."] cs
            else if justLength == -1
                then alt currentScope $ setVariable o (joinCmList $ drop justBegin listA)
                else alt currentScope $ setVariable o (joinCmList $ take justLength $ drop justBegin listA)
list ["FIND", l, value, o] _ = getList l >>= (\ls -> alt currentScope $ setVariable o (BS.pack $ show $ fromMaybe (-1) (elemIndex value ls)))
list ("APPEND" : l : args) _ = getList l >>= (\ls -> alt currentScope $ setVariable l (joinCmList $ ls ++ args)) -- TODO optimize
list ("INSERT" : l : idxS : args) cs = case readMaybe $ BS.unpack idxS of
    Nothing -> liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Cannot INSERT as index cannot be parsed."] cs
    Just idx -> getList l >>= (\(beg, end) -> alt currentScope $ setVariable l (joinCmList $ beg ++ args ++ end)) . splitAt idx
list ("POP_BACK" : _ : _) cs =
    liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("POP_FRONT" : _ : _) cs =
    liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REMOVE_AT" : _ : _) cs =
    liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REMOVE_DUPLICATES" : _ : _) cs =
    liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("REVERSE" : _ : _) cs =
    liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["Not implemented"] cs
list ("PREPEND" : l : args) _ = getList l >>= (\ls -> alt currentScope $ setVariable l (joinCmList $ args ++ ls)) -- TODO optimize

list _ cs = raiseArgumentCountError "list" cs

-- NOT IMPLEMENTED: (POP_BACK), (POP_FRONT)
-- TODO (REMOVE_AT), (REMOVE_DUPLICATES), (REVERSE)


-- UTILITY
getList :: ByteString -> Interp [ByteString]
getList l = splitCmList . fromMaybe "" . readVariable l <$> sel currentScope
