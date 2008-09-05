-- | High level interface to Glpk.
module Glpk.Types where

import Data.Set( Set, union, unions )
import GHC.Exts( IsString(..) )
import qualified Data.Set as Set

data StandardLP = StandardLP
    { objective :: Objective
    , subjectTo :: [LinearExpr]
    , bounds    :: [Bound] } deriving (Eq, Ord, Show)

data Objective = Objective Dir LinearExpr deriving (Eq, Ord, Show)
data Dir = Maximize | Minimize deriving (Eq, Ord, Show)

data LinearExpr = LinearExpr [(Coeff, Var)] deriving (Eq, Ord, Show)

-- | A bound on a value.
data Bound = Free Value
           -- | @Free v@ is the bound @-oo < v < &#8734;@.
           | Lower Double Value
           | Upper Value Double
           -- | @Upper v i@ represents @-oo < v <= i@.
           | Double Double Value Double
           | Fixed Double Value
           -- | A bound fixing a `Value' to a given `Double'.
             deriving (Eq, Ord, Show)

-- | A bounded value can be either a constraint or a variable.
data Value = ValueConstr LinearExpr | ValueVar    Var
             deriving (Eq, Ord, Show)

newtype Var = Var String deriving (Eq, Ord, Show)
instance IsString Var where fromString = Var

type Coeff = Double

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


------------------------------------------------------------------------------
-- Solution repr

data LPSolution = LPSolution deriving (Eq, Ord, Show)
