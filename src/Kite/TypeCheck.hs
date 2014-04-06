{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Parser

import Data.Maybe
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

-- traceShow initSymbols $ return()

stdBinOpArgs = PFuncType [PFreeType "a", PFreeType "a"] (PFreeType "a")
initSymbols = Map.fromList [("+",stdBinOpArgs),
                            ("-",stdBinOpArgs),
                            ("*",stdBinOpArgs),
                            ("/",stdBinOpArgs),
                            ("%",stdBinOpArgs),
                            ("==",stdBinOpArgs),
                            ("<",stdBinOpArgs),
                            ("<=",stdBinOpArgs),
                            (">",stdBinOpArgs),
                            (">=",stdBinOpArgs),
                            ("!=",stdBinOpArgs)]



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

newtype TypeEnvironment = TypeEnvironment (Map.Map String Scheme)

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

infer :: TypeEnvironment -> Expr -> TC (Substitution, Type)

infer (TypeEnvironment _) (PInteger _) = return (nullSubst, PIntegerType)
infer (TypeEnvironment _) (PFloat _) = return (nullSubst, PFloatType)
infer (TypeEnvironment _) (PString _) = return (nullSubst, PStringType)

infer env (PBlock StandardBlock exprs) = infer env (head exprs)
infer env (PBlock FuncBlock exprs) = infer env (head exprs)

infer (TypeEnvironment env) (PIdentifier ide) =
  case Map.lookup ide env of
    Just f -> do
      t <- instantiate f
      return (nullSubst, t)
    Nothing -> error ("Undefined variable: " ++ ide)

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
  (envParams, tParams) <- foldM (\(envAcc, tParams) p -> do
                         tParam <- freshFtv "t"
                         let PTypeArg _ (PIdentifier ide) = p
                             TypeEnvironment env' = remove envAcc ide
                             env'' = TypeEnvironment (Map.insert ide (Scheme [] tParam) env')
                         return (env'', tParams ++ [tParam])
                     ) (env, []) params
  (s1, t1) <- infer envParams body
  return (s1, PFuncType (apply s1 tParams) t1)

infer env (PCall expr args) = do
  fresh <- freshFtv "t"
  (sFn, tFn) <- infer env expr
  let env' = apply sFn env
  (sArgs, tArgs) <- foldM (\(s, tArgs') p -> do
                                  (sArg, tArg) <- infer env' p
                                  return (sArg `composeSubst` s, tArgs' ++ [tArg])
                                  ) (nullSubst, []) args
  s3 <- unify (apply sArgs tFn) (PFuncType tArgs fresh)
  return (sFn `composeSubst` sArgs `composeSubst` s3, apply s3 fresh)

infer (TypeEnvironment env) (PAssign (PIdentifier ide) expr) = do
  (s1, t1) <- infer (TypeEnvironment env) expr
  let env' = TypeEnvironment (Map.insert ide (Scheme [] t1) env)
  return (nullSubst, apply s1 t1)

unify :: Type -> Type -> TC Substitution

unify ta (PFreeType ide) = varBind ide ta

unify (PFreeType ide) tb = varBind ide tb

unify (PListType ta) (PListType tb) = unify ta tb

unify (PFuncType paramsA ra) (PFuncType paramsB rb) = do
  --s1 <- unify (head pa) (head pb)
  sParams <- foldM (\s (paramA, paramB) -> do
                       s' <- unify paramB paramA
                       return (s `composeSubst` s')
                   ) nullSubst (zip paramsA paramsB)
  s2 <- unify (apply sParams ra) (apply sParams rb)
  return (s2 `composeSubst` sParams)

unify ta tb = throwTE $ printf "Type mismatch in unification (%s and %s)" (show ta) (show tb)

varBind :: Name -> Type -> TC Substitution
varBind ide t | PFreeType ide == t = return nullSubst
              | otherwise = return $ Map.singleton ide t

insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}
{-

-- applySubstEnv :: Substitution -> TC ()
-- applySubstEnv s = do
--   env <- get
--   let sym' = map (Map.map (applySubst s)) (sym env)
--   put env{sym = sym'}

-- -- substitue a ftv into a type
-- -- for instance (a: Int) into (List Free(a)) yields (List Int)
-- applySubst :: Frame -> Type -> Type

-- applySubst fr (PFreeType ide) =
--   fromMaybe (PFreeType ide) (Map.lookup ide fr)

-- applySubst fr (PListType a) = do
--   let ta = applySubst fr a
--   PListType ta

-- applySubst fr (PFuncType params ret) = do
--   let params' = map (applySubst fr) params
--       ret' = applySubst fr ret
--   PFuncType params' ret'

-- applySubst _ t = t

-- stack manipulation
----------------------------
-- sym

popSymF :: TC ()
popSymF = do
  env <- get
  put env{sym = tail (sym env)}

pushEmptySymF :: TC ()
pushEmptySymF = pushSymF Map.empty

pushSymF :: Frame -> TC ()
pushSymF f = do
  env <- get
  put env{sym = f:sym env}

-- frame manipulation
----------------------------
insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}

removeSym :: String -> TC ()
removeSym ide = do
  env <- get
  let (x:xs) = sym env
      newF = Map.delete ide x
  put env{sym = newF:xs}

overrideSym :: String -> Type -> TC ()
overrideSym ide val = do
  removeSym ide
  insertSym ide val

-- operators that return bool
boolOps = ["==", ">", ">=", "<", "<=", "!="]

{-
  for floats and integers, all binary operators are allowed
  for booleans only the boolOps are allowed
-}

-------------------
-- HELPERS
-------------------
isFunc (PFunc _ _) = True
isFunc _ = False

isListType (PListType _) = True
isListType _ = False

isFuncType (PFuncType _ _) = True
isFuncType _ = False

isIntegerType PIntegerType = True
isIntegerType _ = False

isFreeType (PFreeType _) = True
isFreeType _ = False

isFree t = case t of
  PFreeType _ -> True
  _ -> False

-- generate a fresh ftv if type is free
freshIfFree ty = case ty of
  PFreeType freeIde -> freshFtv freeIde
  _ -> return ty

------------------
-- UNIFICATION
------------------
unify :: Type -> Type -> TC Substitution

unify ta (PFreeType ide) = varBind ide ta

unify (PFreeType ide) tb = varBind ide tb

unify (PListType ta) (PListType tb) = unify ta tb

unify (PFuncType pa ra) (PFuncType pb rb) = do
  paramSubst <- foldM (\acc (p1, p2) -> do
                          s <- unify p1 p2
                          return $ acc `composeSubst` s)
                nullSubst (zip pa pb)
  returnSubst <- unify ra rb
  return $ returnSubst `composeSubst` paramSubst

unify ta tb = do
  when (ta /= tb)
    (throwTE $ printf "Type mismatch in unification (%s and %s)" (show ta) (show tb))

  return nullSubst

varBind :: Name -> Type -> TC Substitution
varBind ide t | PFreeType ide == t = return nullSubst
              | otherwise = return $ Map.singleton ide t

-- Get the type and ftv substitutions of an expression
-------------------------------------------------------
typeOf :: Expr -> TC (Substitution, Type)

-- Base cases
---------------
typeOf (PInteger _) = return (nullSubst, PIntegerType)
typeOf (PFloat _)   = return (nullSubst, PFloatType)
typeOf (PString _)  = return (nullSubst, PStringType)
typeOf (PBool _)    = return (nullSubst, PBoolType)

-- Compound types
-------------------
typeOf (PFunc (PFuncType params rty) body) = do
  -- generate fresh ftv names for free parameters
  params' <- mapM (\(PTypeArg ty (PIdentifier ide)) ->
                    case ty of
                      PFreeType freeIde -> freshFtv freeIde >>= \t -> return (ide, t)
                      _ -> return (ide, ty)
                  ) params

  rty' <- freshIfFree rty

  pushSymF (Map.fromList params')

  (s1, bodyTy) <- typeOf body

  s2 <- unify bodyTy rty'

  popSymF

  let s = s1 `composeSubst` s2

  return (s, applySubst s (PFuncType (map snd params') bodyTy))

{-
typeOf (PBinOp op lhs rhs) = do
  (s1, tyLhs) <- typeOf lhs
  (s2, tyRhs) <- typeOf rhs

  s3 <- unify (applySubst s2 tyLhs) (applySubst s2 tyRhs)

  let retTy = if op `elem` boolOps then PBoolType else tyLhs

  -- unless ((retTy, op, retTy) `elem` binOpLookup
  --         || retTy `elem` [PIntegerType,PFloatType] -- All binary ops are allowed for Int and Float
  --         || op `elem` boolOps) -- All boolean ops are allowed for all types
  --   (throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'."
  --    op (show tyRhs) (show tyLhs))

  return (s1 `composeSubst` s2 `composeSubst` s3, applySubst s3 retTy)
  -- unless (tyLhs == tyRhs || (tyLhs, op, tyRhs) `elem` binOpLookup)
  --   (throwTE $ printf "Binary operand types do not match (%s %s %s)."
  --    (show tyLhs) op (show tyRhs))
-}

typeOf (PList []) = do
  fresh <- freshFtv "t"
  return (nullSubst, PListType fresh)

typeOf (PList (x:xs)) = do
  (sHead, tHead) <- typeOf x
  (composedSubst, composedType) <- foldM (\(s, t) el -> do
            (s', t') <- typeOf el

            sUnify <- unify t (applySubst s t')

            when (applySubst sUnify t /= applySubst sUnify t')
              (throwTE $ printf "Varying types in list. Got %s(s) and %s(s)."
               (show t) (show t'))

            return (s `composeSubst` s', applySubst sUnify t)
        ) (sHead, tHead) xs

  return (composedSubst, PListType composedType)

typeOf (PIf cond conseq alt) = do
  (s0, tyCond) <- typeOf cond

  s1 <- unify tyCond PBoolType

  when (applySubst s1 tyCond /= PBoolType)
    (throwTE $ printf "Expected if-condition to be of type Bool, got %s." (show tyCond))

  (s2, tyConseq) <- typeOf conseq

  (s3, tyAlt) <- typeOf alt

  s4 <- unify (applySubst s3 tyAlt) (applySubst s3 tyConseq)
--TODO: catch unification error
  -- when (applySubst s4 tyConseq /= applySubst s4 tyAlt)
  --   (throwTE $ printf "Consequence and alternative in if-expression do not match (%s and %s)."
  --    (show tyConseq) (show tyAlt))

  return (s0 `composeSubst` s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4, applySubst s4 tyConseq)

-- using let rec for functions
typeOf (PAssign (PIdentifier ide) val@(PFunc _ _)) = do
  -- add a ftv for fn types, to allow recursion
  fresh <- freshFtv "t"
  insertSym ide fresh

  (s1, tyVal) <- typeOf val

  env <- get
  let existing = Map.lookup ide (head (sym env))
  let Just tyExisting = existing

  s2 <- unify tyExisting tyVal

  when (isJust existing && applySubst s2 tyExisting /= applySubst s2 tyVal)
        (throwTE $ printf "Reassigning variable '%s' of type %s with type %s."
         ide (show tyExisting) (show tyVal))

  let s3 = s1 `composeSubst` s2

  return (s3, applySubst s3 tyVal)

-- using normal let for everything else
typeOf (PAssign (PIdentifier ide) val) = do
  (s, tyVal) <- typeOf val

  env <- get
  let existing = Map.lookup ide (head (sym env))

  unless (isNothing existing)
        (let Just tyExisting = existing
         in when (applySubst s tyExisting /= applySubst s tyVal)
            (throwTE $ printf "Reassigning variable '%s' of type %s with type %s."
             ide (show tyExisting) (show tyVal)))

  insertSym ide (applySubst s tyVal)

  return (s, applySubst s tyVal)

typeOf (PIndex arr idx) = do
  (s1, tyArr) <- typeOf arr
  (s2, tyIdx) <- typeOf idx

  unless (isIntegerType tyIdx)
    (throwTE $ printf "Invalid index type, expected Int, got %s." (show tyIdx))

  fresh <- freshFtv "t"
  s3 <- unify tyArr (PListType fresh)

  let s = s1 `composeSubst` s2 `composeSubst` s3
      tyArr' = applySubst s tyArr

  unless (isListType tyArr')
    (throwTE $ printf "The index operator is only defined for List # Int, got %s # %s"
     (show tyArr') (show tyIdx))

  let PListType tyItem = tyArr'
  --traceShow tyArr' $ return ()
  return (s, applySubst s tyItem)

-- fold a block and continously update the environment
typeOf (PBlock StandardBlock exprs) = do
  foldM_ (\rets expr -> do
             (s, ty) <- typeOf expr

             applySubstEnv s

             let updatedRets = case expr of
                   PReturn _ -> ty : rets
                   _ -> rets

             return updatedRets
         ) [] exprs

  return (nullSubst, PBoolType)

typeOf (PBlock FuncBlock exprs) = do
  (rets, composedSubst) <- foldM (\(rets, s) el -> do
            (s', t') <- typeOf el

            applySubstEnv s'

            -- save type if it's a return type
            let rets' = case el of
                  PReturn _ -> t' : rets
                  _ -> rets

            return (rets', s `composeSubst` s')
        ) ([], nullSubst) exprs

  --applySubstEnv composedSubst

  when (null rets)
    (throwTE "Missing return statement.")

  -- unify return types
  --ss <- unify (rets !! 1) (rets !! 0)
  let srets = map (applySubst composedSubst) rets

  forM_ srets (\ty -> when (ty /= head srets)
--  forM_ srets (\ty -> when (ty /= head srets)
                (throwTE "Varying return types in block."))

  --let s' = composedSubst `composeSubst` ss
  return (composedSubst, applySubst composedSubst (head srets))

typeOf (PCall expr args) = do
  (s1, tyFunc) <- typeOf expr

  argTypes <- mapM typeOf args

  -- unify function
  fresh <- freshFtv "t"
  s2 <- unify tyFunc (PFuncType ((snd . unzip) argTypes) fresh)

  let tyFunc' = applySubst s2 tyFunc

  unless (isFuncType tyFunc' && (not . isFree) tyFunc')
    (throwTE $ printf "Expression '%s' is not a function." (show tyFunc'))

  let (PFuncType params retTy) = tyFunc'

  when (length params /= length args)
    (throwAE (show tyFunc') (length params) (length args))

  argSubsts <- foldM (\acc (arg, tyParam) -> do
            (s2', tyArg) <- typeOf arg

            s3 <- unify tyArg tyParam
            --TODO:catch the error in unification
              -- (throwTE $ printf "Wrong type of argument when calling function '%s'. Expected %s, got %s."
              --  ide (show tyParam) (show tyArg))
            --let s3' = s `composeSubst` s3

            return $ acc `composeSubst` s3 `composeSubst` s2'
        ) s2 (zip args params)

  let s = s1 `composeSubst` s2 `composeSubst` argSubsts
  return (s2, applySubst s retTy)

typeOf (PIdentifier ide) = do
  env <- get
  let val = findit (sym env)

  when (isNothing val)
    (throwRE $ printf "Reference to undefined variable '%s'." ide)

  let Just ty = val
  return (nullSubst, ty)

  where findit stack =
          if null stack
          then Nothing
          else let (x:xs) = stack
                   val = Map.lookup ide x
               in if isNothing val
                  then findit xs
                  else val

typeOf (PReturn expr) = typeOf expr

-- catch all
typeOf _ = throwError UnknownError
-}
