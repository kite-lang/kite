module Kite.Sugar where

import Kite.Syntax
import Prelude hiding (id)

-- | Desugar infix function call
mkInfixCall op a1 = PApply (PApply (PIdentifier op) a1)
-- | Desugar partial (right) infix function call
mkPartialRightInfixCall op rhs =
  PLambda "rhs" (PReturn (PApply (PApply (PIdentifier op) (PIdentifier "rhs")) rhs))
-- | Desugar partial (left) infix function call
mkPartialLeftInfixCall op = PApply (PIdentifier op)

-- | Desugar String to list of Chars
mkCharList str = PList (map PChar str)

-- | Desugar multiple arguments to partial application
mkCalls f [] = PApply f PVoid
mkCalls f (a:as) = foldl PApply (PApply f a) as

-- | Desugar multiple parameters to curried version
mkFunc [] body = PLambda "Void" body
mkFunc ps body =
  let ls = map PLambda ps
  in foldl1 (\fn l -> fn . PReturn . l) ls body

-- | Desugar to implicit return last expr
mkBlock [] = PReturn PVoid
mkBlock exprs =
  case last exprs of
    PReturn _ -> PBlock exprs
    _ -> PBlock (init exprs ++ [PReturn (last exprs)])

-- | Desugar list comprehensions
mkComprehension expr draws guards =
  let ids = map (\(PDraw id _) -> id) draws
      expr' = mkFunc ids $ mkBlock [expr]
      guards' = map (\guard -> mkFunc ids $ mkBlock [guard]) guards
      ranges = map (\(PDraw _ range) -> range) draws
  in generateFlatmaps ids ranges ids guards' expr'

-- | Generate flatmap of a list comprehension
generateFlatmaps [] _ ids_all guardFuncs finalFunc =
  let args = map PIdentifier ids_all
  in PIf (generateGuards args guardFuncs) (PList [mkCalls finalFunc args]) (PList [])

-- | Generate multiple flatmaps of a list comprehension
generateFlatmaps (id:ids) (r:rs) ids_all guard finalFunc =
  let flatMaps = mkBlock [generateFlatmaps ids rs ids_all guard finalFunc]
  in PApply (PApply (PIdentifier "flatMap") (mkFunc [id] flatMaps)) r

-- | Generate guards for list comprehension
generateGuards _ [] = PBool True
generateGuards args [g] = mkCalls g args
generateGuards args (g:gs) = PApply (PApply (PIdentifier "&&") (mkCalls g args)) (generateGuards args gs)
