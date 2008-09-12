module Main where

import Control.Monad( forM_ )
import Data.List( intercalate )
import Foreign
import Foreign.C
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Glpk
import Glpk.Examples.GlpkExample( problem )
import Glpk.Raw
import Puzzler.Anagram( anagramsPat, shareAna )

shareAnagramer = unsafePerformIO shareAna

main :: IO ()
main = do
    anagramsPat shareAnagramer "aoeu" "?" -- to load the dictionary
      `seq` initGUI
    Just xml <- xmlNew "gui/puzzler.glade"
    window   <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit
    widgetShowAll window
    lettersEntry <- xmlGetWidget xml castToEntry "lettersEntry"
    patternEntry <- xmlGetWidget xml castToEntry "patternEntry"
    anagramResultTextView <- xmlGetWidget xml castToTextView "anagramResultTextView"
    findAnagramsButton <- xmlGetWidget xml castToButton "findAnagramsButton"
    statusLabel <- xmlGetWidget xml castToLabel "statusLabel"
    let setStatus = makeSetStatus statusLabel

    -- Register events:
    onClicked findAnagramsButton
      (doFindAnagrams setStatus lettersEntry patternEntry anagramResultTextView)

    mainGUI -- always last

doFindAnagrams setStatus lettersEntry patternEntry resultTextView =  do
    letters <- get lettersEntry entryText
    pattern <- get patternEntry entryText
    let findingText = "Finding anagrams for '" ++ letters
                      ++ "' of length " ++ show (length pattern) ++ "..."
    setStatus findingText
    mainIteration -- TODO is this the right way to get the status to display
                  -- before finding the anagrams?
    let grams = anagramsPat shareAnagramer letters pattern
    setStatus $ findingText ++ "done."
    buffer <- get resultTextView textViewBuffer
    textBufferSetText buffer $ intercalate " " grams

makeSetStatus statusLabel newText = set statusLabel [ labelText := newText ]

main' :: IO ()
main' = do
    lp <- c_glp_create_prob
    title <- newCString "the title"
    ss@[p,q,r,x1,x2,x3] <-
        mapM newCString ["p", "q", "r", "x1", "x2", "x3"] :: IO [CString]
    c_glp_set_prob_name lp title
    c_glp_set_obj_dir lp dirMax
    c_glp_add_rows lp 3
    c_glp_set_row_name lp 1 p
    c_glp_set_row_bnds lp 1 glpUp 0 100
    c_glp_set_row_name lp 2 q
    c_glp_set_row_bnds lp 2 glpUp 0 600
    c_glp_set_row_name lp 3 r
    c_glp_set_row_bnds lp 3 glpUp 0 300

    c_glp_add_cols lp 3

    c_glp_set_col_name lp 1 x1
    c_glp_set_col_bnds lp 1 glpLo 0 0
    c_glp_set_col_name lp 2 x2
    c_glp_set_col_bnds lp 2 glpLo 0 0
    c_glp_set_col_name lp 3 x3
    c_glp_set_col_bnds lp 3 glpLo 0 0

    c_glp_set_obj_coef lp 3 4

    ia <- newArray [0,1,1,1,2,3,2,3,2,3] -- 0 is padding, and etc.
    ja <- newArray [0,1,2,3,1,1,2,2,3,3]
    ar <- newArray [0,1,1,1,10,2,4,2,5,6]
    c_glp_load_matrix lp 9 ia ja ar
    c_glp_simplex lp nullPtr
    z <- c_glp_get_obj_val lp
    x1' <- c_glp_get_col_prim lp 1
    x2' <- c_glp_get_col_prim lp 2
    x3' <- c_glp_get_col_prim lp 3
    putStrLn $
      "z = " ++ show z
      ++ " x1 = " ++ show x1'
      ++ " x2 = " ++ show x2'
      ++ " x3 = " ++ show x3'

    c_glp_delete_prob lp
    free title
    forM_ ss free
    free ia
