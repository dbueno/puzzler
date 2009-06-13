-- | High level interface to Glpk.
module Glpk.Types where

import Data.Array.IArray
import Data.Array.Unboxed( UArray )

-- | As in glpk, all arrays of n elements are indexed from 1 to n.
data StandardLP = StandardLP
    { objective :: Objective
    , coeffs    :: CoeffMatrix
    -- ^ Problem constraint coefficients are in rows.  The columns correspond to
    -- structural variables.
    , constraintBounds :: Bounds
    , problemVarBounds :: Bounds }
                  deriving (Eq, Ord, Show)

data Objective = Objective{ objDir    :: Dir
                          , objCoeffs :: UArray Int Coeff }
                 deriving (Eq, Ord, Show)
data Dir = Maximize | Minimize deriving (Eq, Ord, Show)

type CoeffMatrix = Array Int (UArray Int Coeff)
type Bounds = Array Int Bound

type Coeff = Double

-- | A bound on a value.
data Bound = Free
           -- ^ @Free v@ is the bound @-oo < v < &#8734;@.
           | Lower Double
           -- ^ @Lower i@ represents @i <= v < +oo@.
           | Upper Double
           -- ^ @Upper i@ represents @-oo < v <= i@.
           | Double Double Double
           -- ^ @Double lb ub@ represents @lb <= v <= ub@.
           | Fixed Double
           -- ^ A bound that fixes the associated value.
             deriving (Eq, Ord, Show)

-- | Verify that the problem is well-formed.
checkStandardLP :: StandardLP -> Bool
checkStandardLP lp =
    let b  = bounds (objCoeffs . objective $ lp)
        r@(rowBeg, rowEnd) = bounds (coeffs lp)
    in b == bounds (problemVarBounds lp)
       && (rowBeg, rowEnd) == bounds (constraintBounds lp)
       && fst b == 1
       && rowBeg == 1
       && ((`all` range r) $ \r -> bounds ((coeffs lp)!r) == b)

lpRows, lpCols :: StandardLP -> Int
lpRows lp = let (b, e) = bounds (coeffs lp)
            in e - b + 1
lpCols lp = let (b, e) = bounds (objCoeffs . objective $ lp)
            in e - b + 1

{-
class Vars a where
    -- | Returns a list of all variables used in the problem.
    vars :: a -> Set Var

instance Vars StandardLP where
    vars lp = vars (objective lp)
              `union` concatMap vars (subjectTo lp)
              `union` concatMap vars (bounds lp)
      where
        concatMap :: (Ord b) => (a -> Set b) -> [a] -> Set b
        concatMap f xs = unions $ map f xs

instance Vars Objective where vars (Objective _ expr) = vars expr
instance Vars LinearExpr where
    vars (LinearExpr coeffsAndVars) = Set.fromList $ map snd coeffsAndVars
instance Vars Bound where vars (Free v)       = vars v
                          vars (Lower _ v)    = vars v
                          vars (Upper v _)    = vars v
                          vars (Double _ v _) = vars v
                          vars (Fixed _ v)    = vars v
instance Vars Value where vars (ValueConstr expr) = vars expr
                          vars (ValueVar v) = Set.singleton v
-}

------------------------------------------------------------------------------
-- Solution repr

data LPSolution =
    LPSolution{ objectiveVal   :: Double
              , problemVarVals :: Array Int Double }
    deriving (Eq, Ord, Show)
