-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derived from works under BSD-3-Clause Copyright (c) 2019, Georg Rudoy
-- https://hackage.haskell.org/package/cmake-syntax-0.1.0.0
-- Modifications include fixing separatedArguments and tweaking the AST
-- to only and better encode the information needed by an interpreter
--
-- AST and parser for CMake condition syntax
----------------------------------------------------------------------------

module CMake.Cond (
  module CMake.Cond.Defs,
  module CMake.Cond.Parser
  ) where
import           CMake.Cond.Defs
import           CMake.Cond.Parser
