{-# LANGUAGE OverloadedStrings #-}

import           CMake.Interpreter.Arguments
import           CMake.Interpreter.State
import           Data.ByteString.Char8       (ByteString)
import qualified Test.QuickCheck             as QC
import qualified Test.QuickCheck.Instances   ()

prop_SetHasUnset :: ByteString -> ByteString -> Bool
prop_SetHasUnset s v = hasVariable s swv && not (hasVariable s (unsetVariable s swv))
  where
    swv = setVariable s v emptyScope

prop_SetRead :: ByteString -> ByteString -> Bool
prop_SetRead s v = Just v == readVariable s (setVariable s v emptyScope)

prop_Braced :: ByteString -> ByteString -> Bool
prop_Braced p i = Just i == braced p (mconcat [p, "{", i, "}"])

main :: IO ()
main = mconcat $ QC.quickCheck <$> [prop_SetHasUnset, prop_SetRead, prop_Braced]
