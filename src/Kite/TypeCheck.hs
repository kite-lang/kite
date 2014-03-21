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
runTC f = runState (runErrorT f) Environment { ftv = [Map.empty],
                                               sym = [Map.empty],
                                               symCount = 0 }

typeCheck :: Expr -> Either TypeError Bool
typeCheck expr =
  let (r, env) = runTC (typeOf expr)
  in case traceShow env r of
    Right _ -> Right True
    Left err -> Left err


----------------
-- ENVIRONMENT
----------------
type Name = String
type Frame = Map.Map Name Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 ftv :: Stack,
                                 symCount :: Int}

instance Show Environment where
  show env = "SYMs: " ++ show (sym env) ++ "   FTVs: " ++ show (ftv env)

-- the monad in which all the state is kept and errors are thrown
type TC a = ErrorT TypeError (State Environment) a

-- generate a fresh type variable
freshFtv :: String -> TC Type
freshFtv ide = do
  env <- get
  let count = symCount env
  put env{symCount =  succ count}
  return $ PFreeType (ide ++ show count)

-- substitue a type variable into the top symbol frame
substTopF :: TC ()
substTopF = do
  env <- get
  let topSyms = (head . sym) env
      tailSyms = (tail . sym) env
      ftvs = (head . ftv) env
      newF = Map.map (substT ftvs) topSyms
  put env{sym = newF : tailSyms}
--popEmptyFtvF

-- substitue a ftv into a type
-- for instance (a: Int) into (List Free(a)) yields (List Int)
substT :: Frame -> Type -> Type

substT fr (PFreeType ide) =
  fromMaybe (PFreeType ide) (Map.lookup ide fr)

substT fr (PListType a) = do
  let ta = substT fr a
  PListType ta

substT fr (PFuncType params ret) = do
  let params' = map (substT fr) params
      ret' = substT fr ret
  PFuncType params' ret'

substT _ t = t

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

-- ftv
popFtvF :: TC ()
popFtvF = do
  env <- get
  put env{ftv = tail (ftv env)}

pushEmptyFtvF :: TC ()
pushEmptyFtvF = pushFtvF Map.empty

pushFtvF :: Frame -> TC ()
pushFtvF f = do
  env <- get
  put env{ftv = f:ftv env}


-- stack querying
----------------------------
--TODO: get the potential bound type if free
typeOfFree :: Type -> TC Type
typeOfFree ty = case ty of
        PFreeType ide -> do
          r <- lookupFtv ide
          case r of
            Just t -> return t
            _ -> return ty
        _ -> return ty

lookupSym :: String -> TC (Maybe Type)
lookupSym ide = do
  env <- get
  let ty = findit $ sym env
  case ty of
    Just (PFreeType freeIde) -> do
      f <- lookupFtv freeIde
      case f of
        Just bound -> return (Just bound)
        Nothing -> return ty
    _ -> return ty
  where
    findit stack =
      if null stack
        then Nothing
        else let (x:xs) = stack
                 val = Map.lookup ide x
             in if isNothing val
                then findit xs
                else val

lookupFtv :: String -> TC (Maybe Type)
lookupFtv ide = do
  env <- get
  return $ findit $ ftv env
  where
    findit stack =
      if null stack
        then Nothing
        else let (x:xs) = stack
                 val = Map.lookup ide x
             in if isNothing val
                then findit xs
                else val

-- frame manipulation
----------------------------
insertFtv :: String -> Type -> TC ()
insertFtv ide val = do
  env <- get
  let (x:xs) = ftv env
      newF = Map.insert ide val x
  put env{ftv = newF:xs}

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

-- arithmic operations look-up

binOpLookup = [(PStringType, "+",PStringType),
               (PListType PStringType,"+",PListType PStringType),
               (PListType PIntegerType,"+",PListType PIntegerType),
               (PListType PFloatType,"+",PListType PFloatType),
               (PListType PBoolType,"+",PListType PBoolType),
               (PListType PStringType,"+",PStringType),
               (PListType PIntegerType,"+", PIntegerType),
               (PListType PFloatType,"+", PFloatType),
               (PListType PBoolType,"+", PBoolType),
               (PStringType,"+",PListType PStringType),
               (PIntegerType,"+",PListType PIntegerType),
               (PFloatType,"+",PListType PFloatType),
               (PBoolType,"+",PListType PBoolType),
               (PFreeType "","+",PFreeType "")]
{-
  for floats and integers, all binary operators are allowed
  for booleans only the boolOps are allowed
-}

-------------------
-- HELPERS
-------------------
extractReturnType rets expr = do
  ty <- typeOf expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return updatedRets

isListType (PListType _) = True
isListType _ = False

isFuncType (PFuncType _ _) = True
isFuncType _ = False

isIntegerType PIntegerType = True
isIntegerType _ = False

isFreeType (PFreeType _) = True
isFreeType _ = False

-- instantiate a new free type
-- instantiate :: String -> Type -> Environment
-- instantiate ide = insertFtv

isFree t = case t of
  PFreeType _ -> True
  _ -> False

-- unify two types
unify :: Type -> Type -> TC Type

unify (PFreeType ide) tb = do
    insertFtv ide tb
    return tb

unify ta tb =
  if isFree tb
    then unify tb ta
    else do unless (ta == tb)
              (throwTE $ printf "Type mismatch in unification (%s and %s)" (show ta) (show tb))
            return ta

instantiate :: String -> Type -> TC ()
instantiate = overrideSym

-- Get the type of an expression
----------------------------------
typeOf :: Expr -> TC Type

-- Base cases
---------------
typeOf (PInteger _) = return PIntegerType
typeOf (PFloat _)   = return PFloatType
typeOf (PString _)  = return PStringType
typeOf (PBool _)    = return PBoolType

-- Compound types
-------------------
typeOf (PFunc (PFuncType params retType) body) = do
  let paramTypes = map (\(PTypeArg ty _) -> ty) params
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) params

  -- TODO: give fresh names same free type identifiers
  pushSymF frame

  bodyRetType <- typeOf body

  -- TODO: SUBSTITUE GOD DAMN FREE TYPES FROM FTV
  --forM params (\param -> overrideSym)

  popSymF

  return (PFuncType paramTypes bodyRetType)

typeOf (PBinOp op lhs rhs) = do
  tyLhs <- typeOf lhs
  tyRhs <- typeOf rhs

  unified <- unify tyLhs tyRhs

  let retTy = if op `elem` boolOps then PBoolType else unified

  unless ((retTy, op, retTy) `elem` binOpLookup
          || retTy `elem` [PIntegerType,PFloatType] -- All binary ops are allowed for Int and Float
          || op `elem` boolOps) -- All boolean ops are allowed for all types
    (throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'."
     op (show tyRhs) (show tyLhs))

  return retTy
  -- unless (tyLhs == tyRhs || (tyLhs, op, tyRhs) `elem` binOpLookup)
  --   (throwTE $ printf "Binary operand types do not match (%s %s %s)."
  --    (show tyLhs) op (show tyRhs))

typeOf (PList (x:xs)) = do
  tyHead <- typeOf x
  mapM_ (\i -> do
            ty <- typeOf i
            when (ty /= tyHead)
              (throwTE $ printf "Varying types in list. Got %s(s) and %s(s)."
               (show tyHead) (show ty))
        ) xs
  return (PListType tyHead)

typeOf (PIf cond conseq alt) = do
  tyCond <- typeOf cond

  when (PBoolType /= tyCond)
    (throwTE $ printf "Expected if-condition to be of type Bool, got %s." (show tyCond))

  --TODO: handle if blocks by push/pop of frames
  tyConseq <- typeOf conseq
  tyAlt <- typeOf alt

  when (tyConseq /= tyAlt)
    (throwTE $ printf "Consequence and alternative in if-expression do not match (%s and %s)."
     (show tyConseq) (show tyAlt))

  return tyConseq

-- push a frame of ftv, type check function and apply the inferred types
typeOf (PAssign (PIdentifier ide) func@(PFunc funcTy@(PFuncType params retType) _)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params

  pushEmptyFtvF

  insertSym ide (PFuncType tyParams retType)

  inferredFuncType <- typeOf func

  overrideSym ide inferredFuncType

  substTopF

--  popFtvF

  return inferredFuncType

typeOf (PAssign (PIdentifier ide) val) = do
  tyVal <- typeOf val
  existing <- lookupSym ide

  unless (isNothing existing)
    (let Just tyExisting = existing
     in when (tyExisting /= tyVal)
        (throwTE $ printf "Reassigning variable '%s' of type %s with type %s."
         ide (show tyExisting) (show tyVal)))

  insertSym ide tyVal
  return tyVal

typeOf (PIndex arr idx) = do
  tyArr <- typeOf arr
  tyIdx <- typeOf idx

  unless (isIntegerType tyIdx)
    (throwTE $ printf "Invalid index type, expected Int, got %s." (show tyIdx))

  unless (isListType tyArr)
    (throwTE "The index operator is only defined for List # Int.")

  let PListType tyItem = tyArr

  return tyItem

typeOf (PBlock StandardBlock exprs) = do
  _ <- foldM extractReturnType [] exprs
  return PBoolType

typeOf (PBlock FuncBlock exprs) = do
  rets <- foldM extractReturnType [] exprs

  when (null rets)
    (throwTE "Missing return statement.")

  mapM_ (\ty -> when (ty /= head rets) $ throwTE "Varying return types in block.") rets
  return (head rets)

typeOf (PImmCall (PFunc (PFuncType params retType) body) args) = do
  when (length params /= length args) $ throwAE
    "<anonymous>" (length params) (length args)
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) params
  pushSymF frame
  _ <- typeOf body
  mapM_ (\(arg, param) -> do
            let (PTypeArg tyParam _) = param
            tyArg <- typeOf arg
            when (tyArg /= tyParam) $ throwTE $ printf
              "Wrong type of argument. Expected %s, got %s."
              (show tyParam) (show tyArg)
        ) (zip args params)
  return retType

typeOf (PCall ident@(PIdentifier ide) args) = do
  pushEmptyFtvF

  tyFunc <- typeOf ident

  unless (isFuncType tyFunc)
    (throwTE $ printf "Variable '%s' is not a function." ide)

  let (PFuncType params retTy) = tyFunc

  params' <- mapM typeOfFree params

  when (length params' /= length args)
    (throwAE ide (length params') (length args))

  mapM_ (\(arg, tyParam) -> do
            tyArg <- typeOf arg

            unified <- unify tyArg tyParam
            traceShow unified $ return ()
            tyArg' <- typeOfFree tyArg
            tyParam' <- typeOfFree tyArg

            when (tyArg' /= tyParam')
              (throwTE $ printf "Wrong type of argument when calling function '%s'. Expected %s, got %s."
               ide (show tyParam) (show tyArg))
        ) (zip args params')

  --popFtvF

  return retTy

typeOf (PIdentifier ide) = do
  val <- lookupSym ide

  when (isNothing val)
    (throwRE $ printf "Reference to undefined variable '%s'." ide)

  let Just ty = val
  return ty

typeOf (PReturn expr) = typeOf expr

-- catch all
typeOf _ = throwError UnknownError
