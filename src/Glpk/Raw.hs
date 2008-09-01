{-# INCLUDE <glpk.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module Glpk.Raw where

import Foreign
import Foreign.C

type GlpProb = Ptr ()

type Dir = CInt
type Type = CInt

foreign import ccall "glp_create_prob" c_glp_create_prob :: IO GlpProb
foreign import ccall "glp_set_prob_name" c_glp_set_prob_name :: GlpProb -> CString -> IO ()
foreign import ccall "glp_set_obj_dir" c_glp_set_obj_dir :: GlpProb -> CInt -> IO ()
foreign import ccall "glp_add_rows" c_glp_add_rows :: GlpProb -> CInt -> IO ()
foreign import ccall "glp_add_cols" c_glp_add_cols :: GlpProb -> CInt -> IO ()
foreign import ccall "glp_set_row_name" c_glp_set_row_name
    :: GlpProb -> CInt -> CString -> IO ()
foreign import ccall "glp_set_col_name" c_glp_set_col_name
    :: GlpProb -> CInt -> CString -> IO ()
foreign import ccall "glp_set_row_bnds" c_glp_set_row_bnds
    :: GlpProb -> CInt -> Type -> CDouble -> CDouble
foreign import ccall "glp_set_col_bnds" c_glp_set_col_bnds
    :: GlpProb -> CInt -> Type -> CDouble -> CDouble