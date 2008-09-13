module Main where

import Control.Monad( forM_ )
import Data.List( nub, intercalate, sort )
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import qualified Data.ByteString.Char8 as BS

import Puzzler.Anagram( anagramsPat, createDictionary )

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

    -- Register events:
    let dictFile = "data/mball.txt"
    dict <- createDictionary "data/mball.txt"
    anagramsPat dict "aoeu" "?" -- to load the dictionary
      `seq` 
      onClicked findAnagramsButton
      (doFindAnagrams dict setStatus lettersEntry patternEntry anagramResultTextView)
    setStatus $ "Loaded words from '" ++ dictFile ++ "'."

    mainGUI -- always last

doFindAnagrams dict setStatus lettersEntry patternEntry resultTextView =  do
    letters <- get lettersEntry entryText
    pattern <- get patternEntry entryText
    let findingText = "Finding anagrams for '" ++ letters
                      ++ "' of length " ++ show (length pattern) ++ "..."
    setStatus findingText
    let grams = nub . sort $ anagramsPat dict (BS.pack letters) (BS.pack pattern)
    setStatus $ findingText ++ "done."
    buffer <- get resultTextView textViewBuffer
    textBufferSetText buffer $ intercalate " " (map BS.unpack grams)

makeSetStatus statusLabel newText = do
    set statusLabel [ labelText := newText ]
    mainIteration -- TODO is this the right way to get the status to display
                  -- before finding the anagrams?

