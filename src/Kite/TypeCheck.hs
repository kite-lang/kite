{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Parser

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
runTC f = runState (runErrorT f) Environment { sym = [initSymbols],
                                               symCount = Map.size initSymbols }

mkBinopSignature n = let t = PFreeType ("t" ++ n) in PFuncType [t] (PFuncType [t] t)
mkBoolBinopSignature n = let t = PFreeType ("t" ++ n) in PFuncType [t] (PFuncType [t] PBoolType)

initSymbols = do
  let ops = ["+", "-", "*", "/", "%"]
      boolOps = ["==", "<", "<=", ">", ">=", "!="]
      opSigs = map (\(op, n) -> (op, mkBinopSignature (show n))) (zip ops [0 .. length ops])
      boolOpSigs = map (\(op, n) -> (op, mkBoolBinopSignature (show n))) (zip boolOps [length ops ..  length ops + length boolOps])
  Map.fromList (opSigs `union` boolOpSigs)

typeCheck :: Bool -> Expr -> Either TypeError Environment
typeCheck debug expr = do
  --let (r, env) = runTC (typeOf expr)
  let (r, env) = runTC (infer (TypeEnvironment Map.empty) expr)
  --when debug (traceShow env $ return ())
  traceShow r $ return ()
  case r of
    Right _ -> Right env
    Left err -> Left err

----------------
-- ENVIRONMENT
----------------
type Name = String
-- substitution maps a type variable to a type
type Frame = Map.Map Name Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 symCount :: Int}

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
  ftv (PFuncType tParams tRet) = ftv tParams `Set.union` ftv tRet
  ftv _ = Set.empty
  apply s (PFreeType ide) = fromMaybe (PFreeType ide) (Map.lookup ide s)
  apply s (PFuncType tParams tRet) = PFuncType (apply s tParams) (apply s tRet)
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

popSymF :: TC ()
popSymF = do
  env <- get
  put env{sym = tail (sym env)}

infer :: TypeEnvironment -> Expr -> TC (Substitution, Type)

infer _ (PInteger _) = return (nullSubst, PIntegerType)
infer _ (PFloat _) = return (nullSubst, PFloatType)
infer _ (PString _) = return (nullSubst, PStringType)

-- TODO: why have different block types?
infer env (PBlock StandardBlock exprs) = do
  forM_ exprs (infer env)
  return (nullSubst, PBoolType)

infer env (PBlock FuncBlock exprs) = do
  forM_ (init exprs) (infer env)
  (s, t) <- infer env (last exprs)
  return (s, t)

infer (TypeEnvironment env) (PIdentifier ide) = do
  symEnv <- get
  case Map.lookup ide env of
    Just f -> do
      t <- instantiate f
      return (nullSubst, t)
    Nothing -> case Map.lookup ide (head $ sym symEnv) of
      Just t' -> return (nullSubst, t')
      Nothing -> throwRE $ printf "Reference to undefined variable '%s'." ide

infer env (PList elems) = do
  fresh <- freshFtv "t"
  (sElems, tElems) <- foldM (\(s, t) e -> do
                                (se, te) <- infer (apply s env) e
                                s' <- unify te t
                                return (se `composeSubst` s' `composeSubst` s, te)
                            ) (nullSubst, fresh) elems
  return (sElems, PListType (apply sElems tElems))

-- TODO: check return type(?)
infer env (PFunc (PFuncType params ret) body) = do
  let param = head params    
  tParam <- freshFtv "t"
  let PTypeArg _ (PIdentifier ide) = param
      TypeEnvironment env' = remove env ide
      env'' = TypeEnvironment (Map.insert ide (Scheme [] tParam) env')
  (s1, t1) <- infer env'' body
  return (s1, PFuncType [apply s1 tParam] t1)

-- TODO: check arity
infer env (PCall expr args) = do
  fresh <- freshFtv "t"
  (sFn, tFn) <- infer env expr
  let env' = apply sFn env
      arg = head args
  (sArg, tArg) <- infer env' arg
  s3 <- unify (PFuncType [tArg] fresh) (apply sArg tFn) 
  return (sFn `composeSubst` sArg `composeSubst` s3, apply s3 fresh)

infer (TypeEnvironment env) (PAssign (PIdentifier ide) expr) = do
  (s1, t1) <- infer (TypeEnvironment env) expr
  insertSym ide (apply s1 t1)
  return (nullSubst, apply s1 t1)

unify :: Type -> Type -> TC Substitution

unify PIntegerType PIntegerType = return nullSubst
unify PFloatType PFloatType = return nullSubst
unify PStringType PStringType = return nullSubst

unify ta (PFreeType ide) = varBind ide ta

unify (PFreeType ide) tb = varBind ide tb

unify (PListType ta) (PListType tb) = unify ta tb

unify (PFuncType paramsA ra) (PFuncType paramsB rb) = do
  sParam <- unify (head paramsB) (head paramsA)
  s2 <- unify (apply sParam ra) (apply sParam rb)
  return (s2 `composeSubst` sParam)

unify ta tb = throwTE $ printf "Type mismatch in unification (%s and %s)" (show ta) (show tb)

varBind :: Name -> Type -> TC Substitution
varBind ide t | t == PFreeType ide = return nullSubst
              | PFreeType ide == t = return nullSubst
              | otherwise = return $ Map.singleton ide t

insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}
