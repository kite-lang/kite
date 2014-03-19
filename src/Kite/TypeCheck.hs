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

----------------
-- ENVIRONMENT
----------------
type Frame = Map.Map String Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 ftv :: Stack}

instance Show Environment where
  show env = "SYMs: " ++ show (sym env) ++ "   FTVs: " ++ show (ftv env)

-- the monad in which all the type checking happens
type TC a = ErrorT TypeError (State Environment) a

-- stack manipulation
----------------------------
popSymF :: TC ()
popSymF = state $ \env -> let (_:xs) = sym env
                          in ((), env{sym = xs})

pushEmptySymF :: TC ()
pushEmptySymF = pushSymF Map.empty

pushSymF :: Frame -> TC ()
pushSymF f = do
  env <- get
  put env{sym = f:sym env}

-- stack querying
----------------------------
lookupSym :: String -> TC (Maybe Type)
lookupSym ide = do
  env <- get
  return $ findit $ sym env
  where
    findit stack =
      if null stack
        then Nothing
        else let (x:xs) = stack
                 val = Map.lookup ide x
             in if isNothing val
                then findit xs
                else val


countSyms :: TC Int
countSyms = do
  env <- get
  return $ foldl (\acc frame -> acc + Map.size frame) 0 (sym env)


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

-- freshTypeName :: Environment -> String -> String
-- freshTypeName env ide =
--   let count = 5 -- sum $ map length (ftv env)
--   in ide ++ show count

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
-- INTERFACE
-------------------
runTC f = runState (runErrorT f) Environment {ftv = [Map.empty], sym = [Map.empty]}

typeCheck :: Expr -> Either TypeError Bool
typeCheck expr =
  let (r, _) = runTC (typeOf expr)
  in case r of
    Right _ -> Right True
    Left err -> Left err

-------------------
-- HELPERS
-------------------
extractReturnType rets expr = do
  ty <- typeOf expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return updatedRets

-- Constructor checker
------------------------
isListType (PListType _) = True
isListType _ = False

isFuncType (PFuncType _ _) = True
isFuncType _ = False

isIntegerType PIntegerType = True
isIntegerType _ = False

-- throw if not equal
-- tine :: Type -> Type -> TC ()
-- tine ty1 ty2 msg = return $ if ty1 == ty2
--                    then ty1
--                    else throwTE msg


-- instantiate a new free type
-- instantiate :: Environment -> String -> Type -> Environment
-- instantiate = insertType

-- isFree t = case t of
--   PFreeType _ -> True
--   _ -> False

-- unify two types
-- unify :: Environment -> Type -> Type -> TypeCheckMonad (Type, Environment)

-- unify env (PFreeType na) tb =
--   let env' = instantiate env na tb
--   in return (tb, env')

-- unify env ta tb | ta == tb = return (ta, env)
--                 | isFree tb = unify env tb ta
--                 | otherwise = throwTE $ printf "Type mismatch (%s and %s)" (show ta) (show tb)

-- check the type of an expression
typeOf :: Expr -> TC Type

-- base cases
typeOf (PInteger _) = return PIntegerType
typeOf (PFloat _)   = return PFloatType
typeOf (PString _)  = return PStringType
typeOf (PBool _)    = return PBoolType

-- compound types
typeOf (PFunc (PFuncType params retType) body) = do
  let paramTypes = map (\(PTypeArg ty _) -> ty) params
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) params

  -- TODO: give fresh names same free type identifiers
  pushSymF frame
  bodyRetType <- typeOf body

  -- TODO: add instantiated types from function body to current environment but ignore symbols
  when (bodyRetType /= retType)
    (throwTE $ printf "Return type %s does not match annotated type %s."
    (show bodyRetType) (show retType))

  popSymF

  return (PFuncType paramTypes retType)

typeOf (PBinOp op lhs rhs) = do
  tyLhs <- typeOf lhs
  tyRhs <- typeOf rhs

  -- TODO: unify types
  let retTy = if op `elem` boolOps then PBoolType else tyRhs
  unless ((retTy,op,retTy) `elem` binOpLookup ||
          retTy `elem` [PIntegerType,PFloatType] || -- All binary ops are allowed for Int and Float
          op `elem` boolOps) -- All boolean ops are allowed for all types
    (throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'."
     op (show tyRhs) (show tyLhs))

  unless (tyLhs == tyRhs || (tyLhs,op,tyRhs) `elem` binOpLookup)
    (throwTE $ printf "Binary operand types do not match (%s %s %s)."
     (show tyLhs) op (show tyRhs))

  return retTy

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

typeOf (PAssign (PIdentifier ide) func@(PFunc funcTy@(PFuncType params retType) _)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params
  insertSym ide (PFuncType tyParams retType)
  _ <- typeOf func
  return funcTy

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
  --let tyRet = if null rets then PVoidType else head rets
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
  tyFunc <- typeOf ident

  unless (isFuncType tyFunc)
    (throwTE $ printf "Variable '%s' is not a function." ide)

  let (PFuncType params retTy) = tyFunc

  when (length params /= length args)
    (throwAE ide (length params) (length args))

  mapM_ (\(arg, tyParam) -> do
            tyArg <- typeOf arg

            --TODO: unify
            when (tyArg /= tyParam)
              (throwTE $ printf "Wrong type of argument when calling function '%s'. Expected %s, got %s."
               ide (show tyParam) (show tyArg))
        ) (zip args params)

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
