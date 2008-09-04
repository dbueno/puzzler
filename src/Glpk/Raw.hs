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
foreign import ccall "glp_set_obj_coef" c_glp_set_obj_coef :: GlpProb -> CInt -> CDouble -> IO ()
foreign import ccall "glp_set_mat_row" c_glp_set_mat_row
    :: GlpProb -> CInt -> CInt -> Ptr Int -> Ptr CDouble -> IO ()
foreign import ccall "glp_set_mat_col" c_glp_set_mat_col
    :: GlpProb -> CInt -> CInt -> Ptr Int -> Ptr CDouble -> IO ()
foreign import ccall "glp_load_matrix" c_glp_load_matrix
    :: GlpProb -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall "glp_del_rows" c_glp_del_rows :: GlpProb -> CInt -> Ptr CInt -> IO ()
foreign import ccall "glp_del_cols" c_glp_del_cols :: GlpProb -> CInt -> Ptr CInt -> IO ()
foreign import ccall "glp_erase_prob" c_glp_erase_prob :: GlpProb -> IO ()
foreign import ccall "glp_delete_prob" c_glp_delete_prob :: GlpProb -> IO ()

foreign import ccall "glp_get_prob_name" c_glp_get_prob_name :: GlpProb -> IO CString
foreign import ccall "glp_get_obj_name" c_glp_get_obj_name :: GlpProb -> IO CString
foreign import ccall "glp_get_obj_dir" c_glp_get_obj_dir :: GlpProb -> IO Dir
foreign import ccall "glp_get_num_rows" c_glp_get_num_rows :: GlpProb -> IO CInt
foreign import ccall "glp_get_num_cols" c_glp_get_num_cols :: GlpProb -> IO CInt
foreign import ccall "glp_get_row_name" c_glp_get_row_name :: GlpProb -> CInt -> IO CString
foreign import ccall "glp_get_col_name" c_glp_get_col_name :: GlpProb -> CInt -> IO CString
foreign import ccall "glp_get_row_type" c_glp_get_row_type :: GlpProb -> CInt -> IO Type
foreign import ccall "glp_get_row_lb" c_glp_get_row_lb :: GlpProb -> CInt -> IO CDouble
foreign import ccall "glp_get_row_ub" c_glp_get_row_ub :: GlpProb -> CInt -> IO CDouble
foreign import ccall "glp_get_col_type" c_glp_get_col_type :: GlpProb -> CInt -> IO Type
foreign import ccall "glp_get_col_lb" c_glp_get_col_lb :: GlpProb -> CInt -> IO CDouble
foreign import ccall "glp_get_col_ub" c_glp_get_col_ub :: GlpProb -> CInt -> IO CDouble
foreign import ccall "glp_get_obj_coef" c_glp_get_obj_coef :: GlpProb -> CInt -> IO CDouble
foreign import ccall "glp_get_num_nz" c_glp_get_num_nz :: GlpProb -> IO CInt
foreign import ccall "glp_get_mat_row" c_glp_get_mat_row :: GlpProb -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
foreign import ccall "glp_get_mat_col" c_glp_get_mat_col :: GlpProb -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
