-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Extra Monad implements
----------------------------------------------------------------------------
module CMakeHs.Internal.Monad (ifM, (=>>), (<<=)) where

-- | @if@ with a monadic condition
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do c <- b; if c then t else f

-- | Sequentially compose two actions, passing any value produced
-- by the first as an argument to the second.
--
-- \'@as '=>>' bs@\' can be understood as the @do@ expression
--
-- @
-- do a <- as
--    bs a
--    return a
-- @
(=>>) :: Monad m => m a -> (a -> m b) -> m a
(=>>) m f = m >>= (\x -> f x >> return x)

-- | flipped @(=>>)@
(<<=) :: Monad m => (a -> m b) -> m a -> m a
(<<=) = flip (=>>)
