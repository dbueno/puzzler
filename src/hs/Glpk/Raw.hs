{-# INCLUDE <glpk.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module Glpk.Raw where

import Foreign
import Foreign.C

type GlpProb = Ptr ()

type Dir = CInt
type Type = CInt

dirMax = unsafePerformIO c_glp_get_dir_max
dirMin = unsafePerformIO c_glp_get_dir_min

glpFr = unsafePerformIO c_glp_get_glp_fr
glpLo = unsafePerformIO c_glp_get_glp_lo
glpUp = unsafePerformIO c_glp_get_glp_up
glpDb = unsafePerformIO c_glp_get_glp_db
glpFx = unsafePerformIO c_glp_get_glp_fx

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
    :: GlpProb -> CInt -> Type -> CDouble -> CDouble -> IO ()
foreign import ccall "glp_set_col_bnds" c_glp_set_col_bnds
    :: GlpProb -> CInt -> Type -> CDouble -> CDouble -> IO ()
foreign import ccall "glp_set_obj_coef" c_glp_set_obj_coef :: GlpProb -> CInt -> CDouble -> IO ()
foreign import ccall "glp_set_mat_row" c_glp_set_mat_row
    :: GlpProb -> CInt -> CInt -> Ptr Int -> Ptr CDouble -> IO ()
foreign import ccall "glp_set_mat_col" c_glp_set_mat_col
    :: GlpProb -> CInt -> CInt -> Ptr Int -> Ptr CDouble -> IO ()
-- void glp_load_matrix(glp_prob *lp, int ne, const int ia[], 
-- const int ja[], const double ar[]); 
-- Description 
-- The routine glp_load_matrix loads the constraint matrix passed in the 
-- arrays ia, ja, and ar into the specified problem ob ject. Before loading the 
-- current contents of the constraint matrix is destroyed. 
-- Constraint coefficients (elements of the constraint matrix) must be spec- 
-- ified as triplets (ia[k], ja[k], ar[k]) for k = 1, . . . , ne, where ia[k] is 
-- the row index, ja[k] is the column index, and ar[k] is a numeric value of 
-- corresponding constraint coefficient. The parameter ne specifies the total 
-- number of (non-zero) elements in the matrix to be loaded. Coefficients with 
-- identical indices are not allowed. Zero coefficients are allowed, however, they 
-- are not stored in the constraint matrix. 
-- If the parameter ne is 0, the parameters ia, ja, and/or ar can be spec- 
-- ified as NULL. 
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

foreign import ccall "glp_simplex" c_glp_simplex :: GlpProb -> Ptr () -> IO CInt

foreign import ccall "glp_get_obj_val" c_glp_get_obj_val :: GlpProb -> IO CDouble
foreign import ccall "glp_get_col_prim" c_glp_get_col_prim :: GlpProb -> CInt -> IO CDouble


------------------------------------------------------------------------------
-- Macro nonsense

foreign import ccall "glp_get_dir_min" c_glp_get_dir_min :: IO CInt
foreign import ccall "glp_get_dir_max" c_glp_get_dir_max :: IO CInt

foreign import ccall "glp_get_glp_up" c_glp_get_glp_fr :: IO CInt
foreign import ccall "glp_get_glp_lo" c_glp_get_glp_lo :: IO CInt
foreign import ccall "glp_get_glp_up" c_glp_get_glp_up :: IO CInt
foreign import ccall "glp_get_glp_up" c_glp_get_glp_db :: IO CInt
foreign import ccall "glp_get_glp_lo" c_glp_get_glp_fx :: IO CInt

