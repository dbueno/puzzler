module Main where

import Data.Array.IArray
import Glpk
import Puzzler.Conf
import Puzzler.Pretty
import System.Console.GetOpt
import System.IO
-- import System.Log.Logger
import Text.Printf

options :: [OptDescr (Options -> Options)]
options =
  [ 
  ]

data Options = Options
defaultOptions :: Options
defaultOptions = Options

-- Let

--     * x be the number of units of X produced in the current week
--     * y be the number of units of Y produced in the current week

-- then the constraints are:
-- # 50x + 24y <= 40(60) machine A time
-- # 30x + 33y <= 35(60) machine B time
-- # x >= 75 - 30
-- # i.e. x >= 45 so production of X >= demand (75) - initial stock (30), which ensures we meet demand
-- # y >= 95 - 90
-- # i.e. y >= 5 so production of Y >= demand (95) - initial stock (90), which ensures we meet demand

-- The objective is: maximise (x+30-75) + (y+90-95) = (x+y-50)
-- i.e. to maximise the number of units left in stock at the end of the week


-- maximize 
-- z = 10x1 + 6x2 + 4x3 
-- sub ject to 
-- p = x1 + x2 + x3 
-- q = 10x1 + 4x2 + 5x3 
-- r = 2x1 + 2x2 + 6x3 
-- and bounds of variables 
-- -oo < p <= 100
-- 0 <= x1 < oo 
-- -oo < q <= 600
-- 0 <= x2 < oo
-- -oo < r <= 300
-- 0 <= x3 < oo
standardLP =
    let z = listArray (1, 3) [10, 6, 4]
        c = listArray (1, 3) [listArray (1, 3) [1, 1, 1]
                             ,listArray (1, 3) [10, 4, 5]
                             ,listArray (1, 3) [2, 2, 6]]
    in
      StandardLP{ objective = Objective Maximize z
                , coeffs = c
                , constraintBounds =
                    listArray (1, 3) [Upper 100, Upper 600, Upper 300]
                , problemVarBounds =
                    listArray (1, 3) [Lower 0, Lower 0, Lower 0] }


main :: IO ()
main = do
    prepareLoggers
--     (opts, input) <- getArgs >>= validateArgv
    printf "Solving\n%s\n\n" (pretty standardLP)
    solution <- solve standardLP
    print solution
    exitPuzzlerHappy


-- validateArgv :: [Str ing] -> IO (Options, String)
-- validateArgv argv = do
--     case getOpt Permute options argv of
--       (o,[input],[]) -> return (foldl (flip id) defaultOptions o, input)
--       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
--       where header = "Usage: CreatDict [OPTION...] input-words-file"




