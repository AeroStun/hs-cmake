module CMake.Commands.CMakePolicy (cmakePolicy) where
import CMake.Interpreter.State (CmBuiltinCommand)
  
cmakePolicy :: CmBuiltinCommand
cmakePolicy _ _ = pure . Just
