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
module CMakeHs.Internal.Monad (ifM) where

-- | @if@ with a monadic condition
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do c <- b; if c then t else f
