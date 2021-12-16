-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Extra numeric implements
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMakeHs.Internal.Numeric (readMaybeInt) where
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt bs = case BS.readInt bs of Just (v, "") -> Just v; _ -> Nothing
