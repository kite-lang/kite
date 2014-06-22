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
-- | The main function for performing type-check. Takes a debugging
-- flag, and a list of `Declaration`s that will be type checked one by
-- one.
typeCheck :: Bool -> [Decl] -> Either (TypeError, [String]) Environment
typeCheck debug decls = do
  let (r, env) = runTC $ typeCheckDecls decls
      stack = evalTrace env

  when debug (traceShow env $ return ())

  case r of
    Right _ -> Right env
    Left err -> Left (err, stack)

-- | Type check a list of `Declaration`s by folding the list and
-- preserving the state.
typeCheckDecls :: [Decl] -> TC ()
typeCheckDecls decls = do
  forM_ decls (\decl -> case decl of
                  PDecl ide expr -> do

                    pushTrace ("Declaration '" ++ ide ++ "'")

                    syms <- liftM (last . sym) get
                    when (isJust $ Map.lookup ide syms)
                      (throwRE $ printf "Reassigning top level declaration '%s'" ide)

                    when (isLambda expr) $ do
                      fresh <- freshTypeVar "tdecl"
                      insertSym ide fresh

                    (s, t) <- infer (TypeEnvironment Map.empty) expr

                    typeDecl <- lookupType ide

                    ety <- case typeDecl of
                      Just ty -> do
                        ety <- expandAlias ty
                        et <- expandAlias t
                        unify ety et
                          ("Type declaration does not match inferred (%s and %s) of function " ++ ide)
                        return ety
                      Nothing -> return t

                    removeSym ide
                    insertSym ide (apply s ety)

                    popTrace

                  PTypeDecl ide ty -> insertType ide ty

                  PTypeAliasDecl ide ty -> insertTypeAlias ide ty
              )
  return ()

expandAlias :: Type -> TC Type

expandAlias (PAliasType "Bool") = return PBoolType
expandAlias (PAliasType "Int") = return PIntegerType
expandAlias (PAliasType "Float") = return PFloatType
expandAlias (PAliasType "Char") = return PCharType

expandAlias (PAliasType ide) = do
  t <- lookupTypeAlias ide
  case t of
    Just ty -> expandAlias ty
    Nothing -> throwTE ("Unknown type alias '" ++ ide ++ "'")

expandAlias (PPairType a b) = do
  ea <- expandAlias a
  eb <- expandAlias b
  return $ PPairType ea eb

expandAlias (PListType t) = do
  et <- expandAlias t
  return $ PListType et

expandAlias (PLambdaType p r) = do
  ep <- expandAlias p
  er <- expandAlias r
  return $ PLambdaType ep er

expandAlias ty = return ty

-- | Freshen a type by substituing all type vars with fresh one
-- this ensures that callers of the function will not be assigned the same
-- type var thus wrongly constraining a type
freshenType :: Type -> TC Type
freshenType ty = do
  let frees = ftv ty
  newFresh <- mapM (\ft -> do
                       new <- freshTypeVar "t"
                       return (ft, new)) (Set.toList frees)
  let sub = Map.fromList newFresh
  return (apply sub ty)

isLambda (PLambda _ _) = True
isLambda _ = False

isLambdaType (PLambdaType _ _) = True
isLambdaType _ = False

--------------------
-- Type inference --
--------------------
-- | Infer the type of an expression. Takes a type environment for
-- type variables and runs in the `TC` monad so it can lookup types.
-- Returns a substitution derived from the inference that can be
-- applied to other types, and the inferred type.
infer :: TypeEnvironment -> Expr -> TC (Substitution, Type)

-- | Basic primitive types
-- Equivalent to the TAUT of HM
infer _ (PInteger _) = return (nullSubst, PIntegerType)
infer _ (PFloat _) = return (nullSubst, PFloatType)
infer _ (PChar _) = return (nullSubst, PCharType)
infer _ (PBool _) = return (nullSubst, PBoolType)
infer _ (PVoid) = return (nullSubst, PVoidType)

-- | A block returns the type of the last expression in the block and
-- the composition of the substituions from inferring the expressions.
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

-- | Look up an identifier in the environment. Fails if none is found.
-- Equivalent to the INST of HM
infer (TypeEnvironment env) (PIdentifier ide) = do
  pushTrace ("Identifier " ++ ide)

  symEnv <- get
  ty <- case Map.lookup ide env of
    Just f -> instantiate f
    Nothing -> case findit (sym symEnv) of
      Just t' -> return t'
      Nothing -> throwRE $ printf "Reference to undefined variable '%s'" ide

  popTrace

  if isLambdaType ty
    then do t <- freshenType ty
            return (nullSubst, t)
    else return (nullSubst, ty)

  where findit stack =
          if null stack
          then Nothing
          else let (x:xs) = stack
                   val = Map.lookup ide x
               in if isNothing val
                  then findit xs
                  else val

-- | Infer and unify each expression in a list
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

-- | Infer each of the two expressions of a `Pair`
infer env pair@(PPair a b) = do
  pushTrace $ "Pair: " ++ show pair

  (sa, ta) <- infer env a
  (sb, tb) <- infer env b

  popTrace
  return (sa <+> sb, PPairType (apply sa ta) (apply sb tb))

-- | Infers the type of expression to be matched and unifies with the
-- types of patterns to match agains. Also unifies the consequence
-- types and return the final unified type of the consequences.
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

-- | Infer and unify the types of consequent and alternative. Unifies
-- the type of condition with the Bool type.
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

-- | Infers the type of a lambda expression and introduces a new scope
-- when inferring the body.
-- Equivalent to the ABS of HM
infer env (PLambda param body) = do
  pushTrace $ "Lambda: " ++ param ++ " -> ?"

  tParam <- freshTypeVar "t"

  let TypeEnvironment env' = remove env param
      env'' = TypeEnvironment (Map.insert param (Scheme [] tParam) env')
  (s1, t1) <- infer env'' body

  popTrace

  return (s1, PLambdaType (apply s1 tParam) t1)

-- | Infers the resulting type of a function application.
-- Equivalent to the APP of HM
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

-- | Infers the type of an expression and binds that type to an
-- identifiers by inserting it in the type environment.
infer (TypeEnvironment env) (PBind ide expr) = do
  pushTrace $ "Bind: " ++ ide

  -- check that variable is not already defined in topmost scope
  syms <- liftM (head . sym) get
  when (isJust (Map.lookup ide syms) || isJust (Map.lookup ide env))
    (throwRE $ "Redefining variable " ++ ide)

  when (isLambda expr) $ do
    fresh <- freshTypeVar "rec"
    insertSym ide fresh

  (s1, t1) <- infer (TypeEnvironment env) expr

  removeSym ide
  insertSym ide (apply s1 t1)

  popTrace

  return (s1, apply s1 t1)

-- | Infer the type of a return expression and inserts the return type
-- in the current return stack.
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
-- | Unify two types and return a substitution that is the most
-- general unifier. That is, a subsitution that makes the two types
-- equivalent.
unify :: Type -> Type -> String -> TC Substitution

-- | Primitive base cases.
unify a b _ | a == b = return nullSubst

-- | Free type with any type, binds the type var in a substitution
unify ta (PTypeVar ide) _ = varBind ide ta
unify (PTypeVar ide) tb _ = varBind ide tb

-- | Unify the underlying types of two lists
unify (PListType ta) (PListType tb) err = unify ta tb err

-- | Unifies the a and b types of two pairs respectively.
unify (PPairType ta tb) (PPairType ta' tb') err = do
  sa <- unify ta ta' err
  sb <- unify tb tb' err
  return (sa <+> sb)

-- | Unifies the types of two lambdas parameters and return types
unify (PLambdaType paramA ra) (PLambdaType paramB rb) err = do
  sParam <- unify paramB paramA err
  s2 <- unify (apply sParam ra) (apply sParam rb) err
  return (sParam <+> s2)

-- | If none of the above matched it's an error and types cannot be
-- unified.
unify ta tb err = do
  throwTE $ printf err (show ta) (show tb)

-- | Create a singleton substitution and perform the occurs check that
-- checks whether a polymorphic type (a type scheme) includes itself.
varBind :: Name -> Type -> TC Substitution
varBind ide t | t == PTypeVar ide = return nullSubst
              | ide `Set.member` ftv t = throwTE $ "Occurs in type: " ++ ide ++ " vs. " ++ show t
              | otherwise = return $ Map.singleton ide t
