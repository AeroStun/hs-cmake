-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Extra Functor implements
----------------------------------------------------------------------------
module CMakeHs.Internal.Functor ((<$$>), (<&&>)) where

-- | Double fmap
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) m v = fmap m <$> v

-- | Flipped double fmap
(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) v m = fmap m <$> v
