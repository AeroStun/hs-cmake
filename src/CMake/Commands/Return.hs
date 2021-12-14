module CMake.Commands.Return (cmReturn) where
import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmState (evading),
                                          Evasion (Return))

cmReturn :: CmBuiltinCommand
cmReturn [] _ s = pure $ Just s{evading=Return}
cmReturn _ callSite _ = Nothing <$ printErr
  where
    printErr :: IO ()
    printErr = cmFormattedError FatalError
                                (Just "return")
                                "The RETURN command does not accept any arguments"
                                callSite
