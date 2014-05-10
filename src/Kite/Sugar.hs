module Kite.Sugar where

import Kite.Syntax

mkInfixCall op a1 a2 = PApply (PApply (PIdentifier op) a1) a2
-- TODO: "partial infix right" can probably be done cleaner
mkPartialRightInfixCall op rhs = PLambda (PLambdaType (PTypeArg (PTypeVar "t1") (PIdentifier "lhs"))
                                                  (PTypeVar "t2"))
                                       (PReturn (PApply (PApply (PIdentifier op) (PIdentifier "lhs")) rhs))
mkPartialLeftInfixCall op lhs = PApply (PIdentifier op) lhs

mkCharList str = PList (map PChar str)

--TODO: clean parameterless functions up
mkCalls f args =
  if null args
     then PApply f PVoid
     else foldl PApply (PApply f (head args)) (tail args)

mkFunc params body =
  let firstParam = if null params then PTypeArg PVoidType (PIdentifier "_") else head params
      restParams = if null params then [] else tail params
      ini = PLambda (PLambdaType firstParam (PTypeVar "sugarType"))
      (fns, _) = foldl (\(fn, n) param ->
                    (fn . PReturn . PLambda (PLambdaType param (PTypeVar ("sugarType" ++ [n]))), succ n)
                  ) (ini, 'a') restParams
  in fns body

mkFuncBlock exprs =
  case last exprs of
    PReturn _ -> PBlock FuncBlock exprs
    _ -> PBlock FuncBlock (init exprs ++ [PReturn (last exprs)])
