module Main where

import Foreign
import Foreign.C

import Glpk.Raw

main :: IO ()
main = do
    lp <- c_glp_create_prob
    title <- newCString "the title"
    c_glp_set_prob_name lp title
    -- c_glp_set_obj_dir(

    free title