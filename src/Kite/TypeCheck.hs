{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Syntax
import Kite.Environment

import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Printf

---------------
-- Interface --
---------------
typeCheck :: Bool -> [Decl] -> Either TypeError [Decl]
typeCheck debug decls = do
  let (r, env) = runTC $ typeCheckDecls decls
  when debug (traceShow env $ return ())
  case r of
    Right _ -> Right decls
    Left err -> Left err

typeCheckDecls :: [Decl] -> TC ()
typeCheckDecls decls = do
  forM_ decls (\(PDecl ide expr) -> do
                  fresh <- freshTypeVar "tdecl"
                  insertSym ide fresh

                  (s, t) <- infer (TypeEnvironment Map.empty) expr

                  removeSym ide
                  insertSym ide (apply s t)
              )
  return ()

--------------------
-- Type inference --
--------------------
infer :: TypeEnvironment -> Expr -> TC (Substitution, Type)

infer _ (PInteger _) = return (nullSubst, PIntegerType)
infer _ (PFloat _) = return (nullSubst, PFloatType)
infer _ (PChar _) = return (nullSubst, PCharType)
infer _ (PBool _) = return (nullSubst, PBoolType)
infer _ (PVoid) = return (nullSubst, PVoidType)

-- infer env (PBlock StandardBlock exprs) = do
--   forM_ exprs (infer env)
--   return (nullSubst, PBoolType)

infer env (PBlock exprs) = do
  pushSymFrame
  pushReturnFrame

  (s, t) <- foldM (\(s, _) expr -> do
                      (s', t) <- infer (apply s env) expr
                      return (s <+> s', t)
                  ) (nullSubst, PVoidType) exprs
  -- e <- get
  -- let rets = returns e
  --     implicit = apply s t
  --     valid = all (==implicit) (head rets)

  -- unless valid (throwRE $ printf "Varying return types: %s" (show $ head rets))

  popReturnFrame
  popSymFrame

  return (s, apply s t)

infer (TypeEnvironment env) (PIdentifier ide) = do
  pushTrace ("Identifier " ++ ide)
  e <- get
  if onlyFree e
    then liftM ((,) nullSubst) (freshTypeVar "t")
    else do
    symEnv <- get
    ty <- case Map.lookup ide env of
      Just f -> instantiate f
      Nothing -> case findit (sym symEnv) of
        Just t' -> return t'
        Nothing -> throwRE $ printf "Reference to undefined variable '%s'." ide
    popTrace
    return (nullSubst, ty)
  where findit stack =
          if null stack
          then Nothing
          else let (x:xs) = stack
                   val = Map.lookup ide x
               in if isNothing val
                  then findit xs
                  else val

infer env (PList elems) = do
  pushTrace $ "List: " ++ show elems

  fresh <- freshTypeVar "t"
  (sElems, tElems) <- foldM (\(s, t) e -> do
                                (se, te) <- infer (apply s env) e
                                s' <- unify te t "Varying types in list, saw %s and %s"
                                return (se <+> s' <+> s, te)
                            ) (nullSubst, fresh) elems
  popTrace
  return (sElems, PListType (apply sElems tElems))

infer env pair@(PPair a b) = do
  pushTrace $ "Pair: " ++ show pair

  (sa, ta) <- infer env a
  (sb, tb) <- infer env b

  popTrace
  return (sa <+> sb, PPairType (apply sa ta) (apply sb tb))

infer env (PMatch expr patterns) = do
  pushTrace $ "Match: " ++ show expr

  fpat1 <- freshTypeVar "tp1"
  fpat2 <- freshTypeVar "tp2" -- fails if defined after freshCon and freshPat, W.T.F.?
  freshCon <- freshTypeVar "t1"
  freshPat <- freshTypeVar "t2"
  (sExpr, tExpr) <- infer env expr
  ((sElems, sPat), (tConseq, tPat)) <- foldM (\((s, p), (tc, tp)) (pattern, val) -> do
                                let TypeEnvironment en = env
                                    env' = case pattern of
                                      PatCons hd tl -> do
                                        let hdt = (hd, Scheme [] freshPat)
                                            tlt = (tl, Scheme [] (PListType freshPat))
                                        TypeEnvironment (Map.fromList [hdt, tlt] `Map.union` en)

                                      PatPair a b -> do
                                        let ta = (a, Scheme [] fpat1)
                                            tb = (b, Scheme [] fpat2)
                                        TypeEnvironment (Map.fromList [ta, tb] `Map.union` en)

                                      _ -> env

                                (sp, tp') <- case pattern of
                                      PatPrimitive ex -> do
                                        --e <- get
                                        --put e { onlyFree = True }
                                        (sPat, tyPat) <- infer env ex
                                        ss <- unify (apply sPat tyPat) tExpr "Wrong type of pattern expression, saw %s, expected %s"
                                        --put e { onlyFree = False }
                                        return (ss, apply sPat tyPat)

                                      PatCons _ _ -> do
                                        sP <- unify tExpr (PListType freshPat) "Wrong type of pattern expression, saw %s, expected %s"
                                        return (sP, apply s tExpr)

                                      PatPair _ _ -> do
                                        sP <- unify tExpr (PPairType fpat1 fpat2) "Wrong type of pattern expression, saw %s, expected %s"
                                        return (sP, apply s tExpr)

                                      _ -> return (nullSubst, freshPat)

                                (se, te) <- infer (apply s env') val

                                s' <- unify te tc "Types in pattern don't match, saw %s and %s"
                                --ss <- unify tp' tp "Wrong match-pattern type"
                                return ((se <+> s' <+> s, p <+> sp), (te, tp'))
                            ) ((nullSubst, nullSubst), (freshCon, apply sExpr tExpr)) patterns
  popTrace
  return (sPat <+> sElems <+> sExpr, apply (sElems <+> sPat) tConseq)

infer env (PIf cond conseq alt) = do
  pushTrace $ "If: " ++ show cond
  (s0, tyCond) <- infer env cond

  s1 <- unify tyCond PBoolType "Wrong type in if-condition, saw %s, expected %s"

  (s2, tyConseq) <- infer env conseq

  (s3, tyAlt) <- infer env alt

  let s = s2 <+> s3

  s4 <- unify (apply s tyAlt) (apply s tyConseq) "Consequent and alternative of if-expression must match, saw %s and %s"

  popTrace

  return (s0 <+> s1 <+> s2 <+> s3 <+> s4, apply s4 tyConseq)

-- TODO: check return type(?)
infer env (PLambda (PLambdaType param ret) body) = do
  pushTrace $ "Lambda: " ++ show (PLambdaType param ret)

  tParam <- freshTypeVar "t"
  let PTypeArg _ (PIdentifier ide) = param
      TypeEnvironment env' = remove env ide
      env'' = TypeEnvironment (Map.insert ide (Scheme [] tParam) env')
  (s1, t1) <- infer env'' body

  popTrace

  return (s1, PLambdaType (apply s1 tParam) t1)

infer env (PApply expr arg) = do
  case expr of
    PIdentifier ide -> pushTrace $ "Apply: '" ++ ide ++ "'"
    _ -> pushTrace "Apply"

  (sArg, tArg) <- infer env arg
  (sFn, tFn) <- infer (apply sArg env) expr

  fresh <- freshTypeVar "t"
  let err = case expr of
        PIdentifier ide -> "Argument does not match parameter in function '" ++ ide ++ "', saw %s, expected %s"
        _ -> "Argument does not match parameter, expected %s, saw %s"

  s3 <- unify (apply sFn tFn) (PLambdaType tArg fresh) err

  popTrace

  return (s3 <+> sFn <+> sArg, apply s3 fresh)

infer (TypeEnvironment env) (PBind ide expr) = do
  pushTrace $ "Bind: " ++ ide

  fresh <- freshTypeVar "rec"
  insertSym ide fresh

  (s1, t1) <- infer (TypeEnvironment env) expr

  removeSym ide
  insertSym ide (apply s1 t1)

  popTrace

  return (s1, apply s1 t1)

infer env (PReturn expr) = do
  (s, t) <- infer env expr
  env' <- get
  let top = head (returns env')
      new = t : top
  put env'{ returns = new : returns env'}

  return (s, t)

----------------------
-- Type unification --
----------------------
unify :: Type -> Type -> String -> TC Substitution

-- primitive base cases
unify PIntegerType PIntegerType _ = return nullSubst
unify PFloatType PFloatType     _ = return nullSubst
unify PCharType PCharType       _ = return nullSubst
unify PBoolType PBoolType       _ = return nullSubst
unify PVoidType PVoidType       _ = return nullSubst

-- free type with any type
unify ta (PTypeVar ide) _ = varBind ide ta
unify (PTypeVar ide) tb _ = varBind ide tb

-- list
unify (PListType ta) (PListType tb) err = unify ta tb err

-- pair
unify (PPairType ta tb) (PPairType ta' tb') err = do
  sa <- unify ta ta' err
  sb <- unify tb tb' err
  return (sa <+> sb)

-- functions
unify (PLambdaType paramA ra) (PLambdaType paramB rb) err = do
  sParam <- unify paramB paramA err
  s2 <- unify (apply sParam ra) (apply sParam rb) err
  return (sParam <+> s2)

-- if nothing matched it's an error
unify ta tb err = throwTE $ printf err (show ta) (show tb)

varBind :: Name -> Type -> TC Substitution
varBind ide t | t == PTypeVar ide = return nullSubst
              | ide `Set.member` ftv t = throwTE $ "Occurs in type: " ++ ide ++ " vs. " ++ show t
              | otherwise = return $ Map.singleton ide t
