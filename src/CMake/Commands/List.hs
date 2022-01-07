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
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module CMake.Commands.List (list) where
import           CMake.Error                 (CmErrorKind (FatalError),
                                              cmFormattedError,
                                              raiseArgumentCountError,
                                              raiseFatalError)
import           CMake.Interpreter.ArgsParse (ArgsParser, integral, natural,
                                              parseArgs, someArg, unaryArg)
import           CMake.Interpreter.State     (CmBuiltinCommand, Interp, alt,
                                              altVariable, currentScope,
                                              readVariable, sel, setVariable)
import           CMake.List                  (joinCmList, splitCmList)
import           CMakeHs.Internal.Numeric    (readMaybeInt)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Label                  (mkLabels)
import           Data.List                   (elemIndex, nub)
import           Data.Maybe                  (fromMaybe)
import           ParserT                     (item)

data ListLengthArgs = ListLengthArgs { _lenList :: ByteString, _lenOutVar :: ByteString } deriving Show
data ListJoinArgs = ListJoinArgs { _joinList :: ByteString, _joinGlue :: ByteString, _joinOutVar :: ByteString } deriving Show
data ListSubArgs = ListSubArgs { _subList :: ByteString, _subBegin :: Int, _subLength :: Int, _subOutVar :: ByteString } deriving Show
data ListFindArgs = ListFindArgs { _findList :: ByteString, _findVal :: ByteString, _findOutVar :: ByteString } deriving Show
data ListInsArgs = ListInsArgs { _insList :: ByteString, _insIdx :: Int, _insElems :: [ByteString] } deriving Show
data ListRemItArgs = ListRemItArgs { _remItList :: ByteString, _remItValues :: [ByteString] } deriving Show
newtype ListRemDupArgs = ListRemDupArgs { _remDupList :: ByteString } deriving Show
newtype ListRevArgs = ListRevArgs { _revList :: ByteString } deriving Show
mkLabels [''ListLengthArgs, ''ListJoinArgs, ''ListSubArgs, ''ListFindArgs, ''ListInsArgs, ''ListRemItArgs, ''ListRemDupArgs, ''ListRevArgs]

list :: CmBuiltinCommand
list ("LENGTH" : xs) cs = parseArgs "list(LENGTH)" xs parser (ListLengthArgs mempty mempty) cs >>= procLength
  where
    parser :: ArgsParser ByteString ListLengthArgs ()
    parser = unaryArg "list" lenList (const item)
          >> unaryArg "output variable" lenOutVar (const item)
    procLength :: ListLengthArgs -> Interp ()
    procLength ListLengthArgs{..} = fetchList _lenList >>= (\ls -> alt currentScope $ setVariable _lenOutVar (BS.pack . show . length $ ls))

list ["GET", l, iS, o] cs
  | Just i <- readMaybeInt iS = fetchList l >>= (\ls -> alt currentScope $ setVariable o (ls !! i))
  | otherwise = liftIO $ fail "" <* cmFormattedError FatalError (Just "list") ["index for GET is not an integer."] cs
list ("GET" : l : rest) cs = liftIO $ fail "Unimplemented"

list ("JOIN" : xs) cs = parseArgs "list(JOIN)" xs parser (ListJoinArgs mempty mempty mempty) cs >>= procJoin
  where
    parser :: ArgsParser ByteString ListJoinArgs ()
    parser = unaryArg "list" joinList (const item)
          >> unaryArg "glue" joinGlue (const item)
          >> unaryArg "output variable" joinOutVar (const item)
    procJoin :: ListJoinArgs -> Interp ()
    procJoin ListJoinArgs{..} = fetchList _joinOutVar
                            >>= alt currentScope . setVariable _joinOutVar . BS.intercalate _joinGlue

list ("SUBLIST" : xs) cs = parseArgs "list(SUBLIST)" xs parser (ListSubArgs mempty 0 0 mempty) cs >>= procSub
  where
    parser :: ArgsParser ByteString ListSubArgs ()
    parser = unaryArg "list" subList (const item)
          >> unaryArg "begin" subBegin natural
          >> unaryArg "length" subLength integral
          >> unaryArg "output variable" subOutVar (const item)
    procSub :: ListSubArgs -> Interp ()
    procSub lsa@ListSubArgs{..}
      | _subLength == 0 = alt currentScope (setVariable _subOutVar "")
      | _subLength < -1 = raiseFatalError "list(SUBLIST)" ["length: ", BS.pack (show _subLength), " should be -1 or greater"] cs
      | otherwise = fetchList _subList >>= procSub' lsa
    procSub' lsa@ListSubArgs{..} ls
      | _subLength == -1 || _subBegin + _subLength >= length ls = procSub'' lsa (joinCmList $ drop _subBegin ls)
      | otherwise = procSub'' lsa (joinCmList $ take _subLength $ drop _subBegin ls)
    procSub'' :: ListSubArgs -> ByteString -> Interp ()
    procSub'' ListSubArgs{..} v = alt currentScope $ setVariable _subOutVar v

list ("FIND" : xs) cs = parseArgs "list(FIND)" xs parser (ListFindArgs mempty mempty mempty) cs >>= procFind
  where
    parser :: ArgsParser ByteString ListFindArgs ()
    parser = unaryArg "list" findList (const item)
          >> unaryArg "value" findVal (const item)
          >> unaryArg "output variable" findOutVar (const item)
    procFind :: ListFindArgs -> Interp ()
    procFind ListFindArgs{..} = fetchList _findList
                            >>= alt currentScope . setVariable _findOutVar
                                                 . BS.pack
                                                 . show
                                                 . fromMaybe (-1)
                                                 . elemIndex _findVal
list ("APPEND" : l : xs) _ = listVal >>= \v -> if BS.null v
                                               then alt currentScope $ altVariable l (`mappend` BS.cons ';' (joinCmList xs))
                                               else alt currentScope $ setVariable l (joinCmList xs)
  where
    listVal :: Interp ByteString
    listVal = fromMaybe "" . readVariable l <$> sel currentScope

list ("INSERT" : xs) cs = parseArgs "list(INSERT)" xs parser (ListInsArgs mempty 0 mempty) cs >>= procIns
  where
    parser :: ArgsParser ByteString ListInsArgs ()
    parser = unaryArg "list" insList (const item)
          >> unaryArg "begin" insIdx natural
          >> someArg "elements" insElems (const item)
    procIns :: ListInsArgs -> Interp ()
    procIns lsa@ListInsArgs{..} = alt currentScope $ altVariable _insList (spliceCmList lsa)
    spliceCmList :: ListInsArgs -> ByteString -> ByteString
    spliceCmList ListInsArgs{..} v = let (beg, end) = splitAt _insIdx (splitCmList v)
                                     in joinCmList $ beg ++ _insElems ++ end

list ("POP_BACK" : _ : _) cs = raiseFatalError "list" ["Not implemented"] cs

list ("POP_FRONT" : _ : _) cs = raiseFatalError "list" ["Not implemented"] cs

list ("PREPEND" : l : xs) _ = listVal >>= \v -> if BS.null v
                                                then alt currentScope $ altVariable l (mappend (BS.cons ';' (joinCmList xs)))
                                                else alt currentScope $ setVariable l (joinCmList xs)
  where
    listVal :: Interp ByteString
    listVal = fromMaybe "" . readVariable l <$> sel currentScope

list ("REMOVE_ITEM" : xs) cs = parseArgs "list(REMOVE_ITEM)" xs parser (ListRemItArgs mempty mempty) cs >>= procRemIt
  where
    parser :: ArgsParser ByteString ListRemItArgs ()
    parser = unaryArg "list" remItList (const item)
          >> someArg "values" remItValues (const item)
    procRemIt :: ListRemItArgs -> Interp ()
    procRemIt ListRemItArgs{..} = alt currentScope
                                $ altVariable _remItList (joinCmList . filter (not . (`elem` _remItValues)) . splitCmList)

list ("REMOVE_AT" : _ : _) cs = raiseFatalError "list" ["Not implemented"] cs

list ("REMOVE_DUPLICATES" : xs) cs = parseArgs "list(REMOVE_DUPLICATES)" xs parser (ListRemDupArgs mempty) cs >>= procRemDup
  where
    parser :: ArgsParser ByteString ListRemDupArgs ()
    parser = unaryArg "list" remDupList (const item)
    procRemDup :: ListRemDupArgs -> Interp ()
    procRemDup ListRemDupArgs{..} = alt currentScope $ altVariable _remDupList (joinCmList . nub . splitCmList)

list ("REVERSE" : xs) cs = parseArgs "list(REVERSE)" xs parser (ListRevArgs mempty) cs >>= procRev
  where
    parser :: ArgsParser ByteString ListRevArgs ()
    parser = unaryArg "list" revList (const item)
    procRev :: ListRevArgs -> Interp ()
    procRev ListRevArgs{..} = alt currentScope $ altVariable _revList (joinCmList . reverse . splitCmList)

list _ cs = raiseArgumentCountError "list" cs


-- NOT IMPLEMENTED: (POP_BACK), (POP_FRONT)
-- TODO (REMOVE_AT), (REMOVE_DUPLICATES)


-- UTILITY
fetchList :: ByteString -> Interp [ByteString]
fetchList l = splitCmList . fromMaybe "" . readVariable l <$> sel currentScope
