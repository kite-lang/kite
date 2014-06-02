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
  let firstParam = if null params then PTypeArg PVoidType (PIdentifier "Void") else head params
      restParams = if null params then [] else tail params
      ini = PLambda (PLambdaType firstParam (PTypeVar "sugarType"))
      (fns, _) = foldl (\(fn, n) param ->
                    (fn . PReturn . PLambda (PLambdaType param (PTypeVar ("sugarType" ++ [n]))), succ n)
                  ) (ini, 'a') restParams
  in fns body

mkBlock exprs =
  case last exprs of
    PReturn _ -> PBlock exprs
    _ -> PBlock (init exprs ++ [PReturn (last exprs)])


mkCompreExpr expr draws =
  let ids = map (\ (PDraw id _) -> id) draws
      params = map (\id -> PTypeArg (PTypeVar ("t"++id)) (PIdentifier id)) ids
   in mkFunc params (PBlock [(PReturn expr)])

mkCompreGuards exprs guards =
  let ids = map (\(PDraw id _) -> id) guards
      params = map (\id -> PTypeArg (PTypeVar ("t"++id)) (PIdentifier id)) ids
   in map (\expr -> mkFunc params (PBlock [(PReturn expr)])) exprs

mkComprehension func draws guardFuncs =
  let ids = map (\ (PDraw id _) -> id) draws
      params = map (\id -> PTypeArg (PTypeVar ("t"++id)) (PIdentifier id)) ids
      draws_ranges = map(\ (PDraw _ range) -> range) draws
  -- in mkFunc params (PBlock [(PReturn func)])
  in generateFlatmaps ids draws_ranges ids guardFuncs func

generateFlatmaps ids ranges ids_all guardFuncs finalFunc =
  case ids of
    [] -> let args = map (\id -> PIdentifier id) ids_all
          in PIf (generateGuards args guardFuncs) (PList [mkCalls finalFunc args]) (PList [])
              -- in generateGuards args guardFuncs
    (id:_) -> let param = map (\id -> PTypeArg (PTypeVar ("t"++id)) (PIdentifier id)) [id]
              in
               (PApply
                (PApply (PIdentifier "flatMap")
                 (mkFunc param (PBlock
                                [(PReturn (generateFlatmaps (tail ids) (tail ranges) ids_all guardFuncs finalFunc))]))) (head ranges))

-- generateFinalFunc

generateGuards args guardFuncs =
  case length guardFuncs of
    0 -> PApply (PIdentifier "id") (PBool True)
    1 -> mkCalls (head guardFuncs) args
    _ -> PApply (PApply (PIdentifier "&&") (mkCalls (head guardFuncs) args)) (generateGuards args (tail guardFuncs))
