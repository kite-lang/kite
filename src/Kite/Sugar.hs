module Kite.Sugar where

import Kite.Syntax

mkInfixCall op a1 a2 = PCall (PCall (PIdentifier op) a1) a2
-- TODO: "partial infix right" can probably be done cleaner
mkPartialRightInfixCall op rhs = PFunc (PFuncType (PTypeArg (PFreeType "t") (PIdentifier "lhs"))
                                                  (PFreeType "t"))
                                       (PReturn (PCall (PCall (PIdentifier op) (PIdentifier "lhs")) rhs))
mkPartialLeftInfixCall op lhs = PCall (PIdentifier op) lhs

mkCharList str = PList (map PChar str)

--TODO: clean parameterless functions up
mkCalls f args =
  if null args
     then PCall f PVoid
     else foldl PCall (PCall f (head args)) (tail args)

mkFunc params body =
  let firstParam = if null params then PTypeArg PVoidType (PIdentifier "_") else head params
      restParams = if null params then [] else tail params
      ini = PFunc (PFuncType firstParam (PFreeType "t"))
      fns = foldl (\fn param ->
                    fn . PReturn . PFunc (PFuncType param (PFreeType "t"))
                  ) ini restParams
  in fns body

mkFuncBlock exprs =
  case last exprs of
    PReturn _ -> PBlock FuncBlock exprs
    _ -> PBlock FuncBlock (init exprs ++ [PReturn (last exprs)])
