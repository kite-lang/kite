module Kite.TypeCheck where

import Kite.Parser
import Control.Monad.Error
import qualified Data.Map as Map
import Text.Printf

-- error handling
data TypeError = TypeError String
               | ReferenceError String
               | ArityError String
               | UnknownError
               deriving (Show, Eq)

instance Error TypeError where
  noMsg = UnknownError
  strMsg = TypeError

type TypeCheckMonad = Either TypeError

-- symbol table
type SymbolFrame = Map.Map String Type
type Environment = [SymbolFrame]

pushFrame stack frame = frame : stack
popFrame = tail
topFrame = head

insertIde (x:xs) ide ty = Map.insert ide ty x : xs

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
               (PBoolType,"+",PListType PBoolType)]
{-
  for floats and integers, all binary operators are allowed
  for booleans only the boolOps are allowed
-}


-- main interface
typeCheck :: Expr -> TypeCheckMonad (Type, Environment)
typeCheck = typeOf [Map.empty]

-- helpers
retFold (env, rets) expr = do
  (ty, env) <- typeOf env expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return (env, updatedRets)

throwTE = throwError . TypeError
throwRE = throwError . ReferenceError
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

-- throw if not equal
tine ty1 ty2 env msg = if ty1 == ty2
                      then return (ty1, env)
                      else throwTE msg

typeOfIdentifier env ident@(PTerm (PIdentifier ide)) =
  if null env
  then Nothing
  else case Map.lookup ide (topFrame env) of
    Just ty -> return ty
    Nothing -> typeOfIdentifier (popFrame env) ident

-- check the type of an expression
typeOf :: Environment -> Expr -> TypeCheckMonad (Type, Environment)

-- base cases
typeOf env (PTerm (PInteger _)) = return (PIntegerType, env)
typeOf env (PTerm (PFloat _)) = return (PFloatType, env)
typeOf env (PTerm (PString _)) = return (PStringType, env)
typeOf env (PTerm (PBool _)) = return (PBoolType, env)

-- compound types
typeOf env (PFunc (PFuncType args retType) body) = do
  let argTypes = map (\(PTypeArg ty _) -> ty) args
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) args
  let env' = pushFrame env frame
  (bodyRetType, _) <- typeOf env' body
  when (bodyRetType /= retType) $ throwTE $ printf
    "Return type %s does not match annotated type %s."
    (show bodyRetType) (show retType)
  return (PFuncType argTypes retType, env)

typeOf env (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf env lhs
  (tyRhs, _) <- typeOf env rhs
  let retTy = if op `elem` boolOps then PBoolType else tyRhs
  unless ((retTy,op,retTy) `elem` binOpLookup ||
          retTy `elem` [PIntegerType,PFloatType] || -- All binary ops are allowed for Int and Float
          op `elem` boolOps) -- All boolean ops are allowed for all types
    $ throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'."
    op (show tyRhs) (show tyLhs)
  unless (tyLhs == tyRhs || (tyLhs,op,tyRhs) `elem` binOpLookup) $ throwTE $ printf
    "Binary operand types do not match (%s %s %s)."
    (show tyLhs) op (show tyRhs)
  return (retTy, env)

typeOf env (PList (x:xs)) = do
  (tyHead, _) <- typeOf env x
  mapM_ (\i -> do
            (ty, _) <- typeOf env i
            when (ty /= tyHead) $ throwTE $ printf
              "Varying types in list. Got %s(s) and %s(s)."
              (show tyHead) (show ty)
        ) xs
  return (PListType tyHead, env)

typeOf env (PIf cond conseq alt) = do
  (tyCond, _) <- typeOf env cond
  if (not . (==PBoolType)) tyCond
    then throwTE $ printf
         "Expected if-condition to be of type Bool, got %s." (show tyCond)
    else do
      let env' = pushFrame env Map.empty
      (tyConseq, _) <- typeOf env' conseq
      (tyAlt, _) <- typeOf env' alt
      tine tyConseq tyAlt env $ printf
        "Consequence and alternative in if-expression do not match (%s and %s)."
        (show tyConseq) (show tyAlt)

typeOf env (PAssign (PIdentifier ide) func@(PFunc tyFunc@(PFuncType params retType) _)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params
  let env' = insertIde env ide (PFuncType tyParams retType)
  typeOf env' func
  return (tyFunc, env')

typeOf env (PAssign ident@(PIdentifier ide) val) = do
  (tyVal, _) <- typeOf env val
  case typeOfIdentifier env (PTerm ident) of
    Just existingTy -> if existingTy /= tyVal
                       then throwTE $ printf
                            "Reassigning variable '%s' of type %s with type %s."
                            ide (show existingTy) (show tyVal)
                       else return (tyVal, insertIde env ide tyVal) -- UPDATE
    Nothing -> return (tyVal, insertIde env ide tyVal)

typeOf env (PIndex arr idx) = do
  (tyArr, _) <- typeOf env arr
  (tyIdx, _) <- typeOf env idx
  case tyArr of
    PListType itemTy -> if PIntegerType /= tyIdx
                        then throwTE $ printf
                             "Invalid index type, expected Int, got %s."
                             (show tyIdx)
                        else return (itemTy, env)
    _ -> throwTE "The index operator is only defined for List # Int."

typeOf env (PBlock StandardBlock exprs) = do
  (envLast, rets) <- foldM retFold (env, []) exprs
  --let tyRet = if null rets then PVoidType else head rets
  return (PBoolType, envLast)

typeOf env (PBlock FuncBlock exprs) = do
  (envLast, rets) <- foldM retFold (env, []) exprs
  when (null rets) $ throwTE "Missing return statement."
  mapM_ (\ty ->
          when (ty /= head rets) $ throwTE "Varying return types in block."
        ) rets
  return (head rets, envLast)

typeOf env (PImmCall (PFunc (PFuncType params retType) body) args) = do
  when (length params /= length args) $ throwAE
    "<anonymous>" (length params) (length args)
  mapM_ (\(arg, param) -> do
            let (PTypeArg tyParam _) = param
            (tyArg, _) <- typeOf env arg
            when (tyArg /= tyParam) $ throwTE $ printf
              "Wrong type of argument. Expected %s, got %s."
              (show tyParam) (show tyArg)
        ) (zip args params)
  return (retType, env)

typeOf env (PCall ident@(PIdentifier ide) args) = do
  (tyFunc, _) <- typeOf env (PTerm ident)
  let (PFuncType params retTy) = tyFunc
  when (length params /= length args) $ throwAE ide (length params) (length args)
  case tyFunc of
    PFuncType _ _ -> do
      mapM_ (\(arg, tyParam) -> do
                (tyArg, _) <- typeOf env arg
                when (tyArg /= tyParam) $ throwTE $ printf
                  "Wrong type of argument when calling function '%s'. Expected %s, got %s."
                  ide (show tyParam) (show tyArg)
            ) (zip args params)
      return (retTy, env)
    _ -> throwTE $ printf "Variable '%s' is not a function." (show ide)

typeOf env ident@(PTerm (PIdentifier ide)) =
  case typeOfIdentifier env ident of
    Just ty -> return (ty, env)
    _ -> throwRE $ printf "Reference to undefined variable '%s'." ide

typeOf env (PReturn expr) = typeOf env expr

-- catch all
typeOf _ _ = throwError UnknownError
