module Solve24Puzzle (rawSolve, solve, solve24) where

import Data.List (nub)
import Expr (Expr, evalExpr, mkCon)
import ExprToEzpr (toEzpr)
import Ezpr (Ezpr)

-- import ExprToEzpr ()

solve24 :: Real a => [a] -> [Ezpr a]
solve24 = solve (24 :: Rational)

-- solve :: Rational -> [Rational] -> [Expr Rational]
-- solve :: Rational -> [Rational] -> [Expr Rational]
solve :: (Real a, Fractional b, Eq b) => b -> [a] -> [Ezpr a]
solve target pool = nub $ map toEzpr $ rawSolve target pool

rawSolve :: (Real a, Fractional b, Eq b) => b -> [a] -> [Expr a]
rawSolve target pool = filter ((== target) . evalExpr) (allExpressions (map mkCon pool))

allExpressions :: Fractional a => [a] -> [a]
allExpressions [] = []
allExpressions [e] = [e]
allExpressions es = do
    (s1, s2) <- split es
    e1 <- allExpressions s1
    e2 <- allExpressions s2
    [e1 + e2, e1 - e2, e2 - e1, e1 * e2, e1 / e2, e2 / e1]
  where
    split [] = []
    split [_] = []
    split xs = map (\n -> (take n xs, drop n xs)) [1 .. length (xs) - 1]