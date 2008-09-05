
-- | The first LP problem discussed in the GLPK manual.
module Glpk.Examples.GlpkExample where

import Glpk.Types

problem =
    let p = LinearExpr [(1, x1), (1, x2), (1, x3)]
        q = LinearExpr [(10, x1), (4, x2), (5, x3)]
        r = LinearExpr [(2, x1), (2, x2), (6, x3)]
        x1 = "x1"
        x2 = "x2"
        x3 = "x3"
    in
      StandardLP
      { objective = Objective Maximize
                    (LinearExpr [(10, x1), (6, x2), (4, x3)])
      , subjectTo = [p, q, r]
      , bounds = [ Upper (ValueConstr p) 100 -- -oo < p <= 100
                 , Upper (ValueConstr q) 600
                 , Upper (ValueConstr r) 300
                 , Lower 0 (ValueVar x1) -- 0 <= x1 < oo
                 , Lower 0 (ValueVar x2)
                 , Lower 0 (ValueVar x3) ]
      }
