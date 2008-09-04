-- | High level interface to Glpk.
module Glpk.Types where

import Foreign( Ptr )
import GHC.Exts( IsString(..) )

-- Represent LP as a list of IO actions to be executed in order, all operating
-- on the associated Ptr.  This way even though construction will be imperative
-- it can give a pure interface except for one "solve me" function.
newtype LP = LP { unLP :: (Ptr (), [IO ()]) }

data StandardLP = StandardLP
    { objective :: Objective
    , subjectTo :: [LinearConstr]
    , bounds :: [Bound] }

data Objective = Objective Dir LinearExpr
data Dir = Maximize | Minimize

newtype Var = Var String

instance IsString Var where fromString = Var

type Coeff = Integer

data LinearExpr = LinearExpr [(Coeff, Var)]

data LinearConstr = EQ LinearExpr

data Value = ValueConstr LinearConstr
           | ValueVar    Var

data Bound = LTLE BoundEnd Value BoundEnd
           | LELT BoundEnd Value BoundEnd
data BoundEnd = MInfty
              | IntBound Integer
              | Infty


------------------------------------------------------------------------------
-- Solution repr

data LPSolution = LPSolution
