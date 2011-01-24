
-- | A bit of global config for Puzzler.
module Puzzler.Conf where

import Data.Word( Word64, Word32 )
import System.Exit
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO
import Text.Printf

-- | Name of the puzzler logger.
puzzLog :: String
puzzLog = "puzzLog"

puzzMagic :: Word64
puzzMagic = 0x1234beef

dictMagic :: Word32
dictMagic = 0x1

exitPuzzler :: ExitCode -> IO a
exitPuzzler code = do
    infoM puzzLog $ printf "*** exiting puzzler %s" (show code)
    exitWith code

exitPuzzlerHappy, exitPuzzlerSad :: IO a
exitPuzzlerHappy = exitPuzzler ExitSuccess
exitPuzzlerSad   = exitPuzzler (ExitFailure 1)

prepareLoggers :: IO ()
prepareLoggers = do
    s <- streamHandler stderr ERROR
    updateGlobalLogger rootLoggerName (setLevel EMERGENCY)
    updateGlobalLogger puzzLog (setLevel DEBUG . setHandlers [s])
    infoM puzzLog "*** puzzler started"

