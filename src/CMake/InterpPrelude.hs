-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake interpreter prelude
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module CMake.InterpPrelude(cmPrelude) where

import           CMake.Commands
import           CMake.Interpreter.State (CmBuiltinCommand,
                                          CmCommand (CmBuiltinCommand),
                                          CmState (..), currentScope,
                                          emptyState, readVariable,
                                          registerCommand, sel, setVariable)
import           Control.Monad.IO.Class  (liftIO)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import           System.Directory        (exeExtension)
import           System.Info             (os)

cmPrelude :: CmState
cmPrelude = setupGlobal "CMAKE_STATIC_LIBRARY_SUFFIX" (if os == "mingw32" then ".lib" else "")
          $ setupGlobal "CMAKE_STATIC_LIBRARY_PREFIX" (if os /= "mingw32" then "lib" else "")
          $ setupGlobal "CMAKE_SHARED_LIBRARY_SUFFIX" (if os == "mingw32" then ".dll" else "")
          $ setupGlobal "CMAKE_SHARED_LIBRARY_PREFIX" (if os /= "mingw32" then "lib" else "")
          $ setupGlobal "CMAKE_EXECUTABLE_SUFFIX" (BS.pack exeExtension)
          $ setupGlobal "CMAKE_EXECUTABLE_PREFIX" ""
          $ setupOsGlobals
          $ setupVersionGlobals
          $ registerCommand "cmake_policy" (CmBuiltinCommand cmakePolicy)
          $ registerCommand "file" (CmBuiltinCommand file)
          $ registerCommand "math" (CmBuiltinCommand math)
          $ registerCommand "list" (CmBuiltinCommand list)
          $ registerCommand "string" (CmBuiltinCommand string)
          $ registerCommand "message" (CmBuiltinCommand simpleMessage)
          $ registerCommand "dbg_printvar" (CmBuiltinCommand dbgPrintvar)
          $ registerCommand "unset" (CmBuiltinCommand unset)
          $ registerCommand "set" (CmBuiltinCommand set)
          $ registerCommand "include" (CmBuiltinCommand include)
          $ registerCommand "continue" (CmBuiltinCommand cmContinue)
          $ registerCommand "break" (CmBuiltinCommand cmBreak)
          $ registerCommand "return" (CmBuiltinCommand cmReturn) emptyState
  where
    setupGlobal :: ByteString -> ByteString -> CmState -> CmState
    setupGlobal n v s@CmState{currentScope_=ps} = s{currentScope_= setVariable n v ps}

    setupVersionGlobals :: CmState -> CmState
    setupVersionGlobals s = setupGlobal "CMAKE_MAJOR_VERSION" "3"
                          $ setupGlobal "CMAKE_MINOR_VERSION" "22"
                          $ setupGlobal "CMAKE_PATCH_VERSION" "1"
                          $ setupGlobal "CMAKE_TWEAK_VERSION" "0"
                          $ setupGlobal "CMAKE_VERSION" "3.22.1-hs" s

    setupOsGlobals :: CmState -> CmState
    setupOsGlobals s = foldr (`setupGlobal` "1") s osGlobals

    osGlobals :: [ByteString]
    osGlobals = case os of -- Note: Cygwin detection missing
      "mingw32" ->  ["CMAKE_HOST_WIN32", "WIN32"]
      "darwin" -> ["CMAKE_HOST_UNIX", "CMAKE_HOST_APPLE", "UNIX", "APPLE"]
      "linux-android" -> ["CMAKE_HOST_UNIX", "UNIX", "ANDROID"]
      _ -> ["CMAKE_HOST_UNIX", "UNIX"]

    dbgPrintvar :: CmBuiltinCommand
    dbgPrintvar [name] _ = sel currentScope >>= ((\vv -> liftIO $ BS.putStrLn (mconcat [name, ": ", maybe "<unset>" (\v -> mconcat ["\"", v, "\""]) vv])) . readVariable name)
    dbgPrintvar _ _ = fail ""

    simpleMessage :: CmBuiltinCommand
    simpleMessage [] _   = fail ""
    simpleMessage msgs _ = liftIO $ () <$ BS.putStrLn (mconcat msgs)
