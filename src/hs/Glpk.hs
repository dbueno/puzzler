{-# LANGUAGE RankNTypes #-}

module Glpk( solve ) where

import Control.Monad.State.Lazy hiding( forM_ )
import Data.Foldable
import Data.List( genericLength, sort )
import Data.Map( Map, lookup )
import Foreign.C
import Foreign.Marshal.Alloc( free )
import Foreign.Ptr( Ptr, nullPtr )
import Glpk.Types
import Glpk.Raw
import Prelude hiding( lookup )
import qualified Data.Map as Map
import qualified Data.Set as Set

solve :: StandardLP -> IO LPSolution
solve lp = (`evalStateT` emptyMaps) (do
    -- TODO convert lp into a glpk problem using FFI, solve it, and reify the
    -- solution in Haskell.
    glp <- liftIO c_glp_create_prob
    name <- liftIO $ newCString "My LP Problem"
    liftIO $ c_glp_set_prob_name glp name

    glpSetObjective lp glp ; glpSetSubjectTo lp glp ; glpSetBounds lp glp

    liftIO $ c_glp_simplex glp nullPtr

    -- TODO get solution
    sol <- glpGetSolution glp

    liftIO $ c_glp_delete_prob glp
    liftIO $ free name
    freeAll

    return sol)

-- rows are for constraints
-- columns are for problem variables

glpSetObjective :: StandardLP -> GlpProb -> M ()
glpSetObjective lp glp = do
    let Objective dir (LinearExpr terms) = objective lp
    liftIO $ c_glp_set_obj_dir glp (case dir of Maximize -> dirMax ; Minimize -> dirMin)
    let xs = toList $ vars lp
    stash $ Map.fromList (zip (sort xs) [1..] :: [(Var, CInt)])

    -- Add all problem columns:
    liftIO $ c_glp_add_cols glp (genericLength xs)

    -- Set variable names and objective coefficient for each column:
    forM_ (zip [1..] terms) $ \(i, (coeff, v)) -> do
      name <- liftIO (newCString (show v)) >>= stash
      liftIO $ c_glp_set_col_name glp i name
      liftIO $ c_glp_set_obj_coef glp i (realToFrac coeff)

-- | Set the bounds for the given LP problem.
glpSetBounds :: StandardLP -> GlpProb -> M ()
glpSetBounds lp glp = do
    varMap <- retrieve          -- map of Vars to indices
    expMap <- retrieve          -- map of LinearExprs to indices
    let setBounds (ValueVar v) = case lookup v varMap of
          Nothing -> error $ "Variable " ++ show v ++ " referenced but not in map"
          Just i  -> c_glp_set_col_bnds glp i
        setBounds (ValueConstr exp) = case lookup exp expMap of
          Nothing -> error $ "Constraint " ++ show exp ++ " referenced but not in map"
          Just i  -> c_glp_set_row_bnds glp i
    forM_ (bounds lp) $ \b -> liftIO
      (case b of
         Free v         -> setBounds v glpFr 0 0
         Lower lb v     -> setBounds v glpLo (realToFrac lb) 0
         Upper v ub     -> setBounds v glpUp 0 (realToFrac ub)
         Double lb v ub -> setBounds v glpDb (realToFrac lb) (realToFrac ub)
         Fixed b v      -> setBounds v glpFx (realToFrac b) (realToFrac b))

glpSetSubjectTo lp glp = do
    undefined

glpGetSolution = undefined


data Maps = Maps 
    { _expMap :: Map LinearExpr CInt
    , _varMap :: Map Var CInt
    , _allocs :: [GPtr] }
data GPtr = forall a. GPtr (Ptr a)

-- | Frees all memory allocated during interaction with glpk.
freeAll :: M ()
freeAll = do
    gptrs <- gets _allocs
    forM_ gptrs (\(GPtr p) -> liftIO $ free p)
    modify $ \s -> s{ _allocs = [] }

emptyMaps = Maps { _expMap = Map.empty
                 , _varMap = Map.empty
                 , _allocs = [] }

type M = StateT Maps IO

-- | Stash and retrieve values in the monad state.
class Stash a where
    stash    :: a -> M a
    retrieve :: M a
instance Stash (Map LinearExpr CInt) where
    stash m = modify (\s -> s{ _expMap = m }) >> return m
    retrieve = liftM _expMap get
instance Stash (Map Var CInt) where
    stash m = modify (\s -> s{ _varMap = m }) >> return m
    retrieve = liftM _varMap get
instance Stash (Ptr a) where
    -- Stash a pointer that has been heap-allocated on the C side.
    stash ptr = do modify $ \s -> s{ _allocs = (GPtr ptr):_allocs s }
                   return ptr

        
