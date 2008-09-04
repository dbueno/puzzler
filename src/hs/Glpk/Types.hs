-- | High level interface to Glpk.
module Glpk.Types where

import GHC.Exts( IsString(..) )

data StandardLP = StandardLP
    { objective :: Objective
    , subjectTo :: [LinearExpr]
    , bounds :: [Bound] }

data Objective = Objective Dir LinearExpr
data Dir = Maximize | Minimize

data LinearExpr = LinearExpr [(Coeff, Var)]

-- | A bound on a value.
data Bound = LTLE BoundEnd Value BoundEnd
           -- | @LTLE x y z@ represents the bound @x < y <= z@.
         
           | LELT BoundEnd Value BoundEnd
           -- | @LELT x y z@ represents the bound @x <= y < z@.
             
data BoundEnd = MInfty
              | IntBound Integer
              | Infty

-- | A bounded value can be either a constraint or a variable.
data Value = ValueConstr LinearExpr
           | ValueVar    Var



newtype Var = Var String
instance IsString Var where fromString = Var

type Coeff = Integer


------------------------------------------------------------------------------
-- Solution repr

data LPSolution = LPSolution
