module Kite.Sugar where

import Kite.Syntax

mkInfixCall op a1 a2 = PApply (PApply (PIdentifier op) a1) a2
-- TODO: "partial infix right" can probably be done cleaner
mkPartialRightInfixCall op rhs = PLambda "rhs"
                                 (PReturn (PApply (PApply (PIdentifier op) (PIdentifier "rhs")) rhs))
mkPartialLeftInfixCall op lhs = PApply (PIdentifier op) lhs

mkCharList str = PList (map PChar str)

--TODO: clean parameterless functions up
mkCalls f args =
  if null args
     then PApply f PVoid
     else foldl PApply (PApply f (head args)) (tail args)

mkFunc params body =
  let firstParam = if null params then "Void" else head params
      restParams = if null params then [] else tail params
      ini = PLambda firstParam
      (fns, _) = foldl (\(fn, n) param ->
                    (fn . PReturn . PLambda param, succ n)
                  ) (ini, 'a') restParams
  in fns body

mkBlock exprs =
  case last exprs of
    PReturn _ -> PBlock exprs
    _ -> PBlock (init exprs ++ [PReturn (last exprs)])


mkCompreExpr expr draws =
  let ids = map (\ (PDraw id _) -> id) draws
   in mkFunc ids (PBlock [(PReturn expr)])

mkCompreGuards exprs guards =
  let ids = map (\(PDraw id _) -> id) guards
   in map (\expr -> mkFunc ids (PBlock [(PReturn expr)])) exprs

mkComprehension func draws guardFuncs =
  let ids = map (\ (PDraw id _) -> id) draws
      draws_ranges = map(\ (PDraw _ range) -> range) draws
  in generateFlatmaps ids draws_ranges ids guardFuncs func

generateFlatmaps ids ranges ids_all guardFuncs finalFunc =
  case ids of
    [] -> let args = map (\id -> PIdentifier id) ids_all
          in PIf (generateGuards args guardFuncs) (PList [mkCalls finalFunc args]) (PList [])
    (id:_) -> (PApply
                (PApply (PIdentifier "flatMap")
                 (mkFunc [id] (PBlock
                                [(PReturn (generateFlatmaps (tail ids) (tail ranges) ids_all guardFuncs finalFunc))]))) (head ranges))

generateGuards args guardFuncs =
  case length guardFuncs of
    0 -> PApply (PIdentifier "id") (PBool True)
    1 -> mkCalls (head guardFuncs) args
    _ -> PApply (PApply (PIdentifier "&&") (mkCalls (head guardFuncs) args)) (generateGuards args (tail guardFuncs))
