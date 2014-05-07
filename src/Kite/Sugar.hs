module Kite.Sugar where

import Kite.Syntax

mkInfixCall op a1 a2 = PApply (PApply (PIdentifier op) a1) a2
-- TODO: "partial infix right" can probably be done cleaner
mkPartialRightInfixCall op rhs = PLambda (PLambdaType (PTypeArg (PFreeType "t") (PIdentifier "lhs"))
                                                  (PFreeType "t"))
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
      ini = PLambda (PLambdaType firstParam (PFreeType "t"))
      fns = foldl (\fn param ->
                    fn . PReturn . PLambda (PLambdaType param (PFreeType "t"))
                  ) ini restParams
  in fns body

mkFuncBlock exprs =
  case last exprs of
    PReturn _ -> PBlock FuncBlock exprs
    _ -> PBlock FuncBlock (init exprs ++ [PReturn (last exprs)])
