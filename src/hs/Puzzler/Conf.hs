
-- | A bit of global config for Puzzler.
module Puzzler.Conf where

import System.Exit
import System.Log.Logger( infoM )
import Text.Printf

-- | Name of the puzzler logger.
puzzLog :: String
puzzLog = "puzzLog"

exitPuzzler :: ExitCode -> IO a
exitPuzzler code = do
    infoM puzzLog $ printf "*** exiting puzzler %s" (show code)
    exitWith code

exitPuzzlerHappy, exitPuzzlerSad :: IO a
exitPuzzlerHappy = exitPuzzler ExitSuccess
exitPuzzlerSad   = exitPuzzler (ExitFailure 1)
