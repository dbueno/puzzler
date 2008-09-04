module Glpk where

import Glpk.Types
import Glpk.Raw

solve :: StandardLP -> IO LPSolution
solve lp =
    -- TODO convert lp into a glpk problem using FFI, solve it, and reify the
    -- solution in Haskell.
    undefined
