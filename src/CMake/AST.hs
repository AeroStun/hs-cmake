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
-- AST and parser for CMake syntax
----------------------------------------------------------------------------

module CMake.AST (
  module CMake.AST.Defs,
  module CMake.AST.Parser
  ) where
import           CMake.AST.Defs
import           CMake.AST.Parser
