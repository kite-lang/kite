{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Parser

import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
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

throwTE :: String -> TC ()
throwTE = throwError . TypeError

throwRE :: String -> TC ()
throwRE = throwError . ReferenceError

throwAE :: String -> Int -> Int -> TC ()
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
  let (r, env) = runTC (typeOf expr)
  when debug (traceShow env $ return ())
  case r of
    Right _ -> Right env
    Left err -> Left err

----------------
-- ENVIRONMENT
----------------
type Name = String
-- substitution maps a type variable to a type
type Substitution = Map.Map Name Type
type Frame = Map.Map Name Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 symCount :: Int}

instance Show Environment where
  show env = "Top symbol frame\n" ++ foldl (\acc (n, v) -> acc ++ n ++ ":\t" ++ show v ++ "\n") "" (Map.toList . head . sym $ env)

-- the monad in which all the state is kept and errors are thrown
type TC a = ErrorT TypeError (State Environment) a

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
nullSubst = Map.empty
composeSubst s1 s2 = s1 `Map.union` s2

applySubstEnv :: Substitution -> TC ()
applySubstEnv s = do
  env <- get
  let sym' = map (Map.map (applySubst s)) (sym env)
  put env{sym = sym'}

-- substitue a ftv into a type
-- for instance (a: Int) into (List Free(a)) yields (List Int)
applySubst :: Frame -> Type -> Type

applySubst fr (PFreeType ide) =
  fromMaybe (PFreeType ide) (Map.lookup ide fr)

applySubst fr (PListType a) = do
  let ta = applySubst fr a
  PListType ta

applySubst fr (PFuncType params ret) = do
  let params' = map (applySubst fr) params
      ret' = applySubst fr ret
  PFuncType params' ret'

applySubst _ t = t

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
