-- | High level interface to Glpk.
module Glpk.Types where

import Data.Array.IArray
import Data.Array.Unboxed( UArray )
import Puzzler.Pretty
import Data.List( intercalate )

-- | As in glpk, all arrays of @n@ elements are indexed from @1@ to @n@.
data StandardLP = StandardLP
    { objective :: Objective
    , coeffs    :: CoeffMatrix
    , constraintBounds :: Bounds
    , problemVarBounds :: Bounds }
                  deriving (Eq, Ord, Show)        

data Objective = Objective{ objDirection :: Direction
                          , objCoeffs    :: UArray Int Coeff }
                 deriving (Eq, Ord, Show)

data Direction = Maximize | Minimize deriving (Eq, Ord, Show)


-- | Problem constraint coefficients are in rows.  The columns correspond to
-- structural variables.
type CoeffMatrix = Array Int (UArray Int Coeff)

-- | Bounds on either the problem constraints or the problem variables.
type Bounds = Array Int Bound

type Coeff = Double

-- | A bound on a value.
data Bound = Free
           -- ^ @Free v@ is the bound @-&#8734; < v < +&#8734;@.
           | Lower Double
           -- ^ @Lower i@ represents @i <= v < +&#8734;@.
           | Upper Double
           -- ^ @Upper i@ represents @-&#8734; < v <= i@.
           | Double Double Double
           -- ^ @Double lb ub@ represents @lb <= v <= ub@.
           | Fixed Double
           -- ^ A bound that fixes the associated value.
             deriving (Eq, Ord, Show)

-- | Verify that the problem is well-formed.
--
-- This checks that
--
--   * Objective coefficient array, the coefficient matrix, and the bound arrays
--     all have the appropriate dimensions.  Each row in the coefficient matrix
--     must have the same bounds as the objective coefficient array and the
--     bound arrays.
--
--   * All the arrays start at 1 and go up.
wellFormedStandardLP :: StandardLP -> Bool
wellFormedStandardLP lp =
    let b  = bounds (objCoeffs . objective $ lp)
        r = bounds (coeffs lp)
    in b == bounds (problemVarBounds lp)
       && r == bounds (constraintBounds lp)
       && fst b == 1
       && fst r == 1
       && ((`all` range r) $ \r -> bounds ((coeffs lp)!r) == b)

-- | Retrieves the number of rows/cols in the problem.
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



instance Pretty StandardLP where
    pretty lp =
      let o = objective lp
      in
        show (objDirection o) ++ "\n    "
        ++ intercalate " + " (map (\(i, c) -> show c ++ "*x" ++ show i) (assocs $ objCoeffs o))
        ++ "\nSubject to:\n    "
        ++ concatMap (++"\n    ") (map prettyRow (assocs $ coeffs lp))
        ++ concatMap (++"\n    ") (map prettyVarBound (assocs $ problemVarBounds lp))

      where
        prettyRow :: (Int, UArray Int Coeff) -> String
        prettyRow (idx, row) =
            let expr = intercalate " + " $ map (\(i, c) -> show c ++ "*x" ++ show i) (assocs row)
                lb = prettyCLBound (constraintBounds lp!idx)
                ub = prettyCUBound (constraintBounds lp!idx)
            in lb ++ expr ++ ub

        prettyVarBound (i, b) =
            prettyCLBound b
            ++ "x" ++ show i
            ++ prettyCUBound b

        prettyCLBound (Lower x) = show x ++ " <= "
        prettyCLBound (Fixed x) = show x ++ " <= "
        prettyCLBound (Double x _) = show x ++ " <= "
        prettyCLBound _         = ""
        prettyCUBound (Upper x) = " <= " ++ show x
        prettyCUBound (Fixed x) = " <= " ++ show x
        prettyCUBound (Double _ y) = " <= " ++ show y
        prettyCUBound _         = ""

------------------------------------------------------------------------------
-- Solution repr

data LPSolution =
    LPSolution{ objectiveVal   :: Double
              , problemVarVals :: Array Int Double }
    deriving (Eq, Ord, Show)
