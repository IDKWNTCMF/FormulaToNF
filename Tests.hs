module Tests where

import FormulaToNF

-- This function divides formula until the highest operation is "&" and returns pair of left one and everything else
untilLeftAnd :: Formula -> (Bool, Formula, Formula)
untilLeftAnd (f1 :& f2) = let (b, g1, g2) = untilLeftAnd f1 
                          in if b then (True, g1, g2 :& f2) else (True, g1, f2)
untilLeftAnd f          = (False, f, F)

-- This function divides formula until the highest operation is "|" and returns pair of left one and everything else
untilLeftOr :: Formula -> (Bool, Formula, Formula)
untilLeftOr (f1 :| f2) = let (b, g1, g2) = untilLeftOr f1 
                         in if b then (True, g1, g2 :| f2) else (True, g1, f2)
untilLeftOr f          = (False, f, F)

-- This instance is only valid for formulas without implication and double implication and checks whether the order of variables and operations is the same (and variables are the same)
instance Eq Formula where
    (==) (Var a)      (Var b)      = a == b
    (==) T            T            = True
    (==) F            F            = True
    (==) (N f1)       (N f2)       = f1 == f2
    (==) f@(f1 :& f2) g@(g1 :& g2) = (a1 == b1) && (a2 == b2)
                                         where (_, a1, a2) = untilLeftAnd f
                                               (_, b1, b2) = untilLeftAnd g
    (==) f@(f1 :| f2) g@(g1 :| g2) = (a1 == b1) && (a2 == b2)
                                         where (_, a1, a2) = untilLeftOr f
                                               (_, b1, b2) = untilLeftOr g
    (==) _            _            = False

v = Var "v"
x = Var "x"
y = Var "y"
z = Var "z"

-- Tests for formulaToNNF
test0 | formulaToNNF x == x = "Test 0 passed\n"
      | otherwise           = "Test 0 failed\n"
test1 | formulaToNNF T == T = "Test 1 passed\n"
      | otherwise           = "Test 1 failed\n"
test2 | formulaToNNF F == F = "Test 2 passed\n"
      | otherwise           = "Test 2 failed\n"
test3 | formulaToNNF (N (N x)) == x = "Test 3 passed\n"
      | otherwise                   = "Test 3 failed\n"
test4 | formulaToNNF (x :& y) == x :& y = "Test 4 passed\n"
      | otherwise                       = "Test 4 failed\n"
test5 | formulaToNNF (x :| y) == x :| y = "Test 5 passed\n"
      | otherwise                       = "Test 5 failed\n"      
test6 | formulaToNNF (N (x :& y)) == N x :| N y = "Test 6 passed\n"
      | otherwise                               = "Test 6 failed\n"
test7 | formulaToNNF (N (x :| y)) == N x :& N y = "Test 7 passed\n"
      | otherwise                               = "Test 7 failed\n"
test8 | formulaToNNF (x :=> y) == N x :| y = "Test 8 passed\n"
      | otherwise                          = "Test 8 failed\n"
test9 | formulaToNNF (x :<=> y) == (N x :| y) :& (N y :| x) = "Test 9 passed\n"
      | otherwise                                           = "Test 9 failed\n"      
test10 | formulaToNNF (x :& (y :<=> N (z :| y))) == x :& (N y :| N z :& N y) :& (z :| y :| y) = "Test 10 passed\n"
       | otherwise                                                                            = "Test 10 failed\n"
       
 -- Tests for formulaToDNF      
test11 | formulaToDNF (x :& y) == x :& y = "Test 11 passed\n"
       | otherwise                       = "Test 11 failed\n"
test12 | formulaToDNF (x :| y) == x :| y = "Test 12 passed\n"
       | otherwise                       = "Test 12 failed\n"
test13 | formulaToDNF (x :=> y) == N x :| y = "Test 13 passed\n"
       | otherwise                          = "Test 13 failed\n"
test14 | formulaToDNF (x :<=> y) == (N x :& N y) :| (y :& N y) :| (N x :& x) :| (y :& x) = "Test 14 passed\n"
       | otherwise                                                                       = "Test 14 failed\n"
test15 | formulaToDNF (x :& (y :| z)) == (x :& y) :| (x :& z) = "Test 15 passed\n"
       | otherwise                                            = "Test 15 failed\n"
test16 | formulaToDNF ((x :| y) :& z) == (x :& z) :| (y :& z) = "Test 16 passed\n"
       | otherwise                                            = "Test 16 failed\n"
test17 | formulaToDNF ((x :| y) :& (N z :| v)) == (x :& N z) :| (y :& N z) :| (x :& v) :| (y :& v) = "Test 17 passed\n"
       | otherwise                                                                                 = "Test 17 failed\n"

-- Tests for formulaToCNF
test18 | formulaToCNF (x :& y) == x :& y = "Test 18 passed\n"
       | otherwise                       = "Test 18 failed\n"
test19 | formulaToCNF (x :| y) == x :| y = "Test 19 passed\n"
       | otherwise                       = "Test 19 failed\n"
test20 | formulaToCNF (x :=> y) == N x :| y = "Test 20 passed\n"
       | otherwise                          = "Test 20 failed\n"
test21 | formulaToCNF (x :<=> y) == (N x :| y) :& (N y :| x) = "Test 21 passed\n"
       | otherwise                                           = "Test 21 failed\n"
test22 | formulaToCNF (x :| (y :& z)) == (x :| y) :& (x :| z) = "Test 22 passed\n"
       | otherwise                                            = "Test 22 failed\n"
test23 | formulaToCNF ((x :& y) :| z) == (x :| z) :& (y :| z) = "Test 23 passed\n"
       | otherwise                                            = "Test 23 failed\n"
test24 | formulaToCNF ((x :| y) :& (N z :| v)) == (x :| y) :& (N z :| v) = "Test 24 passed\n"
       | otherwise                                                       = "Test 24 failed\n"
test25 | formulaToCNF (x :& (y :<=> N (z :| y))) == x :& (N y :| N z) :& (N y :| N y) :& (z :| y :| y) = "Test 25 passed\n"
       | otherwise                                                                                     = "Test 25 failed\n"
