
-- | High level interface to Glpk.
module Glpk.Types where

import Foreign( Ptr )

-- Represent LP as a list of IO actions to be executed in order, all operating
-- on the associated Ptr.  This way even though construction will be imperative
-- it can give a pure interface except for one "solve me" function.
newtype LP = LP { unLP :: (Ptr (), [IO ()]) }

data Dir = Maximize | Minimize

data LinearExpr = LinearExpr [(Coeff, Var)]

data LinearConstr = EQ { constrExpr :: LinearExpr, constrBound :: Integer }

data Bound = LTLE BoundEnd LinearConstr BoundEnd
data BoundEnd = MInfinity
              | IntBound Integer
              | Infinity


