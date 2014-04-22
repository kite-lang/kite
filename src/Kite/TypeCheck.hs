{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Parser
import Kite.Syntax

import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf

-------------------
-- ERROR HANDLING
-------------------
data TypeError = TypeError String
               | ReferenceError String
               | ArityError String
               | UnknownError
               deriving (Show, Eq)

instance Error TypeError where
  noMsg = UnknownError
  strMsg = TypeError

throwTE :: String -> TC a
throwTE = throwError . TypeError

throwRE :: String -> TC a
throwRE = throwError . ReferenceError

throwAE :: String -> Int -> Int -> TC a
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

-------------------
-- INTERFACE
-------------------
runTC expr f = runState (runErrorT f) Environment { sym = [initSymbols],
                                                    symCount = Map.size initSymbols,
                                                    ast = expr,
                                                    returns = [] }

mkIndexSignature n = let t = PFreeType ("lt" ++ n) in PFuncType (PListType t) (PFuncType PIntegerType t)
mkConsSignature n = let t = PFreeType ("ltt" ++ n) in PFuncType t (PFuncType (PListType t) (PListType t))
mkArithSignature n = let t = PFreeType ("t" ++ n) in PFuncType t (PFuncType t t)
mkEqualitySignature n = let t = PFreeType ("t" ++ n) in PFuncType t (PFuncType t PBoolType)

initSymbols =
  let ops = ["+", "-", "*", "/", "%"]
      opSigs = map (\(op, n) -> (op, mkArithSignature (show n))) (zip ops [0 .. length ops])
  in Map.fromList (opSigs `union` [("<=", mkEqualitySignature (show $ length ops + 1)),
                                   ("==", mkEqualitySignature (show $ length ops + 2)),
                                   ("#", mkIndexSignature (show $ length ops + 3)),
                                   (":", mkConsSignature (show $ length ops + 4)),
                                   ("length", PFuncType (PListType (PFreeType "tlength")) PIntegerType),
                                   ("slice", PFuncType (PListType (PFreeType "tslice")) (PFuncType PIntegerType (PFuncType PIntegerType (PListType (PFreeType "tslice"))))),
                                   ("print", PFuncType (PFreeType "tprint") (PFreeType "tprint"))])

typeCheck :: Bool -> Expr -> Either TypeError Expr
typeCheck debug expr = do
  let (r, env) = runTC expr (infer (TypeEnvironment Map.empty) expr)
  when debug (traceShow env $ return ())
  case r of
    Right _ -> Right $ ast env
    Left err -> Left err

----------------
-- ENVIRONMENT
----------------
type Name = String
-- substitution maps a type variable to a type
type Frame = Map.Map Name Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 symCount :: Int,
                                 ast :: Expr,
                                 returns :: [[Type]] }

-- environment manipulation
pushReturnFrame = do
  env <- get
  put env{returns = [] : returns env}

popReturnFrame = do
  env <- get
  put env{returns = tail (returns env)}

pushSymFrame = do
  env <- get
  put env{sym = Map.empty:sym env}

popSymFrame = do
  env <- get
  put env{sym = tail (sym env)}

instance Show Environment where
  show env = "Top symbol frame\n" ++ foldl (\acc (n, v) -> acc ++ n ++ ":\t" ++ show v ++ "\n") "" (Map.toList . head . sym $ env)

-- the monad in which all the state is kept and errors are thrown
type TC a = ErrorT TypeError (State Environment) a

---------------------------
data Scheme = Scheme [String] Type
              deriving (Show)

newtype TypeEnvironment = TypeEnvironment (Map.Map String Scheme)
                        deriving (Show)

class Types a where
  ftv :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types TypeEnvironment where
  ftv (TypeEnvironment env) = ftv (Map.elems env)
  apply s (TypeEnvironment env) = TypeEnvironment (Map.map (apply s) env)

instance Types Type where
  ftv (PFreeType ide) = Set.singleton ide
  ftv (PListType t) = ftv t
  ftv (PFuncType tParam tRet) = ftv tParam `Set.union` ftv tRet
  ftv _ = Set.empty
  apply s (PFreeType ide) = fromMaybe (PFreeType ide) (Map.lookup ide s)
  apply s (PFuncType tParam tRet) = PFuncType (apply s tParam) (apply s tRet)
  apply s (PListType t) = PListType (apply s t)
  apply s t = t

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
  apply s = map (apply s)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

remove :: TypeEnvironment -> String -> TypeEnvironment
remove (TypeEnvironment env) ide = TypeEnvironment (Map.delete ide env)

generalize :: TypeEnvironment -> Type -> Scheme
generalize env t = Scheme (Set.toList (ftv t Set.\\ ftv env)) t

instantiate :: Scheme -> TC Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (\_ -> freshFtv "t") vars
  let s = Map.fromList (zip vars freshVars)
  return (apply s t)

-- generate a fresh type variable
freshFtv :: String -> TC Type
freshFtv ide = do
  env <- get
  let count = symCount env
  put env{symCount =  succ count}
  return $ PFreeType (ide ++ show count)

-- SUBSTITUTIONS
-------------------
-- substitue a type variable into the top symbol frame
type Substitution = Map.Map Name Type
nullSubst = Map.empty

composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

infer :: TypeEnvironment -> Expr -> TC (Substitution, Type)

infer _ (PInteger _) = return (nullSubst, PIntegerType)
infer _ (PFloat _) = return (nullSubst, PFloatType)
infer _ (PChar _) = return (nullSubst, PCharType)
infer _ (PBool _) = return (nullSubst, PBoolType)
infer _ (PVoid) = return (nullSubst, PVoidType)

infer env (PBlock StandardBlock exprs) = do
  forM_ exprs (infer env)
  return (nullSubst, PBoolType)

infer env (PBlock FuncBlock exprs) = do
  pushSymFrame
  pushReturnFrame

  (s, t) <- foldM (\(s, _) expr -> do
                      (s', t) <- infer (apply s env) expr
                      return (s `composeSubst` s', t)
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
  symEnv <- get
  case Map.lookup ide env of
    Just f -> do
      t <- instantiate f
      return (nullSubst, t)
    Nothing -> case findit (sym symEnv) of
      Just t' -> return (nullSubst, t')
      Nothing -> throwRE $ printf "Reference to undefined variable '%s'." ide

  where findit stack =
          if null stack
          then Nothing
          else let (x:xs) = stack
                   val = Map.lookup ide x
               in if isNothing val
                  then findit xs
                  else val

infer env (PList elems) = do
  fresh <- freshFtv "t"
  (sElems, tElems) <- foldM (\(s, t) e -> do
                                (se, te) <- infer (apply s env) e
                                s' <- unify te t
                                return (se `composeSubst` s' `composeSubst` s, te)
                            ) (nullSubst, fresh) elems
  return (sElems, PListType (apply sElems tElems))

infer env (PIf cond conseq alt) = do
  (s0, tyCond) <- infer env cond

  s1 <- unify tyCond PBoolType

  when (apply s1 tyCond /= PBoolType)
    (throwTE $ printf "Expected if-condition to be of type Bool, got %s." (show tyCond))

  (s2, tyConseq) <- infer env conseq

  (s3, tyAlt) <- infer env alt

  let s = s2 `composeSubst` s3

  s4 <- unify (apply s tyAlt) (apply s tyConseq)

  return (s0 `composeSubst` s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4, apply s4 tyConseq)

-- TODO: check return type(?)
infer env (PFunc (PFuncType param ret) body) = do
  tParam <- freshFtv "t"
  let PTypeArg _ (PIdentifier ide) = param
      TypeEnvironment env' = remove env ide
      env'' = TypeEnvironment (Map.insert ide (Scheme [] tParam) env')
  (s1, t1) <- infer env'' body
  return (s1, apply s1 (PFuncType tParam t1))

infer env (PCall expr arg) = do
  (sArg, tArg) <- infer env arg
  (sFn, tFn) <- infer (apply sArg env) expr

  fresh <- freshFtv "t"
  s3 <- unify (PFuncType (apply sFn tArg) fresh) (apply sFn tFn)

  let s = sFn `composeSubst` sArg `composeSubst` s3
  return (s, apply s fresh)

infer (TypeEnvironment env) (PAssign (PIdentifier ide) expr) = do
  fresh <- freshFtv "rec"
  insertSym ide fresh

  (s1, t1) <- infer (TypeEnvironment env) expr

  removeSym ide
  insertSym ide (apply s1 t1)
  return (s1, apply s1 t1)

infer env (PReturn expr) = do
  (s, t) <- infer env expr
  env' <- get
  let top = head (returns env')
      new = t : top
  put env'{ returns = new : returns env'}
  return (s, t)

unify :: Type -> Type -> TC Substitution

-- primitive base cases
unify PIntegerType PIntegerType = return nullSubst
unify PFloatType PFloatType = return nullSubst
unify PCharType PCharType = return nullSubst
unify PBoolType PBoolType = return nullSubst
unify PVoidType PVoidType = return nullSubst

-- free type with any type
unify ta (PFreeType ide) = varBind ide ta
unify (PFreeType ide) tb = varBind ide tb

-- lists
unify (PListType ta) (PListType tb) = unify ta tb

-- functions
unify (PFuncType paramA ra) (PFuncType paramB rb) = do
  sParam <- unify paramB paramA
  s2 <- unify (apply sParam ra) (apply sParam rb)
  return (s2 `composeSubst` sParam)

-- if nothing matched it's an error
unify ta tb = throwTE $ printf "Type mismatch in unification (%s and %s)" (show ta) (show tb)

varBind :: Name -> Type -> TC Substitution
varBind ide t | t == PFreeType ide = return nullSubst
              | PFreeType ide == t = return nullSubst
              | otherwise = return $ Map.singleton ide t

removeSym :: String -> TC ()
removeSym ide = do
  env <- get
  let (x:xs) = sym env
      newF = Map.delete ide x
  put env{sym = newF:xs}

insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}
