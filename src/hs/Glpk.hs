{-# LANGUAGE RankNTypes #-}

module Glpk( solve ) where

import Control.Monad.State.Lazy hiding( forM_ )
import Data.Array.IArray
import Data.Foldable
import Data.IORef
import Data.List( genericLength, sort )
import Data.Map( Map, lookup )
import Foreign
import Foreign.C( newCString )
import Foreign.Marshal.Alloc( free )
import Foreign.Ptr( Ptr, nullPtr )
import Glpk.Types
import Glpk.Raw
import Prelude hiding( lookup )

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

solve :: StandardLP -> IO LPSolution
solve lp = (`evalStateT` emptyMaps) (do
    when (not $ checkStandardLP lp)
         (error "StandardLP malformed")
    glp <- liftIO c_glp_create_prob
    name <- liftIO $ newCString "My LP Problem"
    liftIO $ c_glp_set_prob_name glp name

    glpSetupProblem lp glp
    glpSetObjective lp glp
    glpSetCoeff lp glp

    -- Solve!
    liftIO $ c_glp_simplex glp nullPtr

    sol <- glpGetSolution lp glp

    liftIO $ c_glp_delete_prob glp >> free name
    freeAll

    return sol)

-- rows are for constraints
-- columns are for problem variables

glpSetupProblem :: StandardLP -> GlpProb -> M ()
glpSetupProblem lp glp = do
    let cb@(cOne, numCols) = bounds (objCoeffs . objective $ lp)
        rb@(rOne, numRows) = bounds (coeffs lp)

    -- Add all problem columns, i.e., problem variables.
    liftIO $ c_glp_add_cols glp (fromIntegral numCols)
    forM_ (range cb) $ \i -> do
        name <- liftIO (newCString $ "x" ++ show i) >>= stash
        -- Set variable names for each column
        liftIO $ c_glp_set_col_name glp (fromIntegral i) name

    -- Add all problem rows, i.e., problem constraints.
    liftIO $ c_glp_add_rows glp (fromIntegral numRows)
    forM_ (range rb) $ \i -> do
        name <- liftIO (newCString $ "r" ++ show i) >>= stash
        -- Set variable names for each row
        liftIO $ c_glp_set_row_name glp (fromIntegral i) name


glpSetObjective :: StandardLP -> GlpProb -> M ()
glpSetObjective lp glp = do
    let Objective dir cs = objective lp
    liftIO $ c_glp_set_obj_dir glp (case dir of Maximize -> dirMax
                                                Minimize -> dirMin)
    -- set each objective expr coefficient
    forM_ (assocs cs) $ \(i, coeff) ->
        liftIO $ c_glp_set_obj_coef glp (fromIntegral i) (realToFrac coeff)

-- | Set the row and column bounds for the given LP problem.
glpSetBounds :: StandardLP -> GlpProb -> M ()
glpSetBounds lp glp = do
    let setRowBnd i = c_glp_set_row_bnds glp i
        setColBnd i = c_glp_set_col_bnds glp i
        setBndWith setBnd b = case b of
            Free         -> setBnd glpFr 0 0
            Lower lb     -> setBnd glpLo (realToFrac lb) 0
            Upper ub     -> setBnd glpUp 0 (realToFrac ub)
            Double lb ub -> setBnd glpDb (realToFrac lb) (realToFrac ub)
            Fixed b      -> setBnd glpFx (realToFrac b) (realToFrac b)

    forM_ (assocs $ constraintBounds lp) $ \(i, bound) ->
        liftIO $ setBndWith (setRowBnd $ fromIntegral i) bound
    forM_ (assocs $ problemVarBounds lp) $ \(i, bound) ->
        liftIO $ setBndWith (setColBnd $ fromIntegral i) bound

-- | Set up the coefficient matrix.
glpSetCoeff lp glp = do
    let numElems = lpRows lp * lpCols lp
    ra <- liftIO (mallocArray (numElems + 1)) >>= stash
    ca <- liftIO (mallocArray (numElems + 1)) >>= stash
    va <- liftIO (mallocArray (numElems + 1)) >>= stash
    cntR <- liftIO $ newIORef 1
    liftIO (
     forM_ (assocs $ coeffs lp) $ \(row, row_i) ->
        forM_ (assocs row_i) $ \(col, coeff) -> do
            cnt <- readIORef cntR
            pokeElemOff ra cnt (fromIntegral col)
            pokeElemOff ca cnt (fromIntegral row)
            pokeElemOff va cnt (realToFrac coeff)
            writeIORef cntR (cnt+1)
     )
    liftIO $ c_glp_load_matrix glp (fromIntegral numElems) ra ca va

glpGetSolution lp glp = do
    let b = (bounds . objCoeffs . objective $ lp)
    z <- liftIO $ c_glp_get_obj_val glp
    colVals <- liftIO . forM (range b) $ c_glp_get_col_prim glp . fromIntegral
    return $ LPSolution{ objectiveVal   = realToFrac z
                       , problemVarVals = listArray b (map realToFrac colVals) }
   



data Maps = Maps 
    { _allocs :: [GPtr] }
data GPtr = forall a. GPtr (Ptr a)

-- | Frees all memory allocated during interaction with glpk.
freeAll :: M ()
freeAll = do
    gptrs <- gets _allocs
    forM_ gptrs (\(GPtr p) -> liftIO $ free p)
    modify $ \s -> s{ _allocs = [] }

emptyMaps = Maps { _allocs = [] }

type M = StateT Maps IO

-- | Stash and retrieve values in the monad state.
class Stash a where
    stash    :: a -> M a
    retrieve :: M a
instance Stash (Ptr a) where
    -- Stash a pointer that has been heap-allocated on the C side.
    stash ptr = do modify $ \s -> s{ _allocs = (GPtr ptr):_allocs s }
                   return ptr
