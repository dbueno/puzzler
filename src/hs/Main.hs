module Main where

import Control.Monad( forM_ )
import Data.IORef
import Data.List( nub, intercalate, sort )
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Exit( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BS

import Puzzler.Anagram( Dictionary, anagramsPat, createDictionary, emptyDictionary )

main :: IO ()
main = do
    initGUI
    Just xml <- xmlNew "gui/puzzler.glade"
    window   <- xmlGetWidget xml castToWindow "mainWindow"
    onDestroy window mainQuit
    widgetShowAll window
    lettersEntry <- xmlGetWidget xml castToEntry "lettersEntry"
    patternEntry <- xmlGetWidget xml castToEntry "patternEntry"
    anagramResultTextView <- xmlGetWidget xml castToTextView "anagramResultTextView"
    findAnagramsButton <- xmlGetWidget xml castToButton "findAnagramsButton"
    statusLabel <- xmlGetWidget xml castToLabel "statusLabel"
    let setStatus = makeSetStatus statusLabel
    fileOpenMenuItem <- xmlGetWidget xml castToMenuItem "fileOpenMenuItem"

    -- Register events:
    dictionaryRef <- newIORef emptyDictionary
    doCreateDictionary dictionaryRef "data/mball.txt" setStatus
--     readIORef dictionaryRef >>= \dict -> 
--       anagramsPat dict "aoeu" "?" -- to load the dictionary
--       `seq` 
    onClicked findAnagramsButton
      (doFindAnagrams setStatus dictionaryRef
                      lettersEntry patternEntry anagramResultTextView)
    
    onActivateLeaf fileOpenMenuItem
      (doOpenFileChooser dictionaryRef (Just window) setStatus)

    mainGUI -- always last

doFindAnagrams setStatus dictionaryRef lettersEntry patternEntry resultTextView =  do
    letters <- get lettersEntry entryText
    pattern <- get patternEntry entryText
    let findingText = "Finding anagrams for '" ++ letters
                      ++ "' of length " ++ show (length pattern) ++ "..."
    setStatus findingText
    dict <- readIORef dictionaryRef
    let grams = nub . sort $ anagramsPat dict (BS.pack letters) (BS.pack pattern)
    setStatus $ "Found " ++ show (length grams) ++ " '" ++ letters ++ "' anagrams."
    buffer <- get resultTextView textViewBuffer
    textBufferSetText buffer $ intercalate " " (map BS.unpack grams)

doOpenFileChooser dictionaryRef maybeParent setStatus = do
    dialog <- fileChooserDialogNew
              (Just "Choose Dictionary")
              maybeParent
              FileChooserActionOpen
              [("Select", ResponseOk)
              ,("Cancel", ResponseCancel)]
    response <- dialogRun dialog
    case response of
      ResponseOk -> do
          maybePath <- fileChooserGetFilename dialog
          case maybePath of
            Just path -> doCreateDictionary dictionaryRef path setStatus
            Nothing -> fatalExceptionDialog maybeParent 1
                       $ "Could not get pathname from chooser dialog, exiting"
      _ -> return ()
    widgetDestroy dialog

doCreateDictionary dictionaryRef path setStatus = do
    setStatus $ "Using dictionary from '" ++ path ++ "'."
    createDictionary path >>= writeIORef dictionaryRef
    

fatalExceptionDialog maybeParent code s = do
    dialog <- messageDialogNew maybeParent [] MessageError ButtonsOk ("FATAL: " ++ s)
    response <- dialogRun dialog
    exitWith (ExitFailure code)
    

makeSetStatus statusLabel newText = do
    set statusLabel [ labelText := newText ]
    mainIteration -- TODO is this the right way to get the status to display
                  -- before finding the anagrams?
