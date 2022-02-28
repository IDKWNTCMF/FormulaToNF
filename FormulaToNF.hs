module FormulaToNF where

type Symb = String

infixl 9 :&
infixl 8 :|
infixl 7 :=>
infixl 6 :<=>

data Formula = Var Symb 
             | T 
             | F
             | N Formula
             | Formula :& Formula 
             | Formula :| Formula 
             | Formula :=> Formula 
             | Formula :<=> Formula
             
instance Show Formula where
    showsPrec _ (Var a)      = showString a
    showsPrec _ T            = showString "true"
    showsPrec _ F            = showString "false"
    showsPrec _ (N (Var a))  = showString $ "~" ++ a
    showsPrec _ (N f)        = showString $ "~(" ++ showsPrec 0 f "" ++ ")"
    showsPrec d (f1 :& f2)   = showString $ if d /= 9 && d /= 0 then "(" ++ showsPrec 9 f1 "" ++ " & " ++ showsPrec 9 f2 "" ++ ")"
                                     else showsPrec 9 f1 "" ++ " & " ++ showsPrec 9 f2 ""
    showsPrec d (f1 :| f2)   = showString $ if d /= 8 && d /= 0 then "(" ++ showsPrec 8 f1 "" ++ " | " ++ showsPrec 8 f2 "" ++ ")"
                                     else showsPrec 8 f1 "" ++ " | " ++ showsPrec 8 f2 ""
    showsPrec _ (f1 :=> f2)  = showString $ "(" ++ showsPrec 7 f1 "" ++ " => " ++ showsPrec 7 f2 "" ++ ")"
    showsPrec _ (f1 :<=> f2) = showString $ "(" ++ showsPrec 6 f1 "" ++ " <=> " ++ showsPrec 6 f2 "" ++ ")"
             
-- Auxiliary function for NNF             
toNNF :: Formula -> Bool -> Formula
toNNF (N f)        isNeg = toNNF f (not isNeg)
toNNF f@(Var a)    isNeg = if isNeg then N f        else f
toNNF T            isNeg = if isNeg then F          else T
toNNF F            isNeg = if isNeg then T          else F
toNNF (f1 :&   f2) isNeg = if isNeg then (a1 :| a2) else (a1 :& a2) 
                               where a1 = toNNF f1 isNeg
                                     a2 = toNNF f2 isNeg
toNNF (f1 :|   f2) isNeg = if isNeg then (a1 :& a2) else (a1 :| a2) 
                               where a1 = toNNF f1 isNeg
                                     a2 = toNNF f2 isNeg
toNNF (f1 :=>  f2) isNeg = toNNF ((N f1) :| f2)               isNeg
toNNF (f1 :<=> f2) isNeg = toNNF ((f1 :=> f2) :& (f2 :=> f1)) isNeg

-- General function for NNF
formulaToNNF :: Formula -> Formula
formulaToNNF f = toNNF f False

-- This function checks whether the highest operation is "|" or not
isDNF :: Formula -> Bool
isDNF (f1 :| f2) = True
isDNF f          = False

-- Auxiliary function for DNF
toDNF :: Formula -> Formula
toDNF (f1 :& (f2 :| f3)) = (toDNF $ a1 :& a2) :| (toDNF $ a1 :& a3) 
                               where a1 = toDNF f1
                                     a2 = toDNF f2
                                     a3 = toDNF f3
toDNF ((f1 :| f2) :& f3) = (toDNF $ a1 :& a3) :| (toDNF $ a2 :& a3) 
                               where a1 = toDNF f1
                                     a2 = toDNF f2
                                     a3 = toDNF f3
toDNF (f1 :| f2)         = a1                 :| a2                 
                               where a1 = toDNF f1
                                     a2 = toDNF f2
toDNF (f1 :& f2)         = if isDNF a1 || isDNF a2 then toDNF (a1 :& a2) else a1 :& a2 
                               where a1 = toDNF f1
                                     a2 = toDNF f2
toDNF f                  = f

-- General function for DNF
formulaToDNF :: Formula -> Formula
formulaToDNF f = toDNF $ formulaToNNF f

-- General function for CNF
formulaToCNF :: Formula -> Formula
formulaToCNF f = formulaToNNF $ N $ formulaToDNF $ N f
