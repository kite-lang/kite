module Kite.TypeCheck (typeCheck) where

import Kite.Parser
import Control.Monad.Error
import qualified Data.Map as Map
import Text.Printf

-- error handling
data TypeError = GenericTE String
               | UnkownTE
               deriving (Show)

instance Error TypeError where
  noMsg = GenericTE "Unknown type error"
  strMsg = GenericTE

type TypeCheckMonad = Either TypeError

-- symbol table
type SymFrame = Map.Map String Type
type SymStack = [SymFrame]

pushFrame stack frame = frame : stack
popFrame = tail
topFrame = head

insertIde (x:xs) ide ty = Map.insert ide ty x : xs

-- operators that return bool
boolOps = ["==", ">", ">=", "<", "<=", "!="]

-- main interface
typeCheck :: Expr -> TypeCheckMonad (Type, SymStack)
typeCheck = typeOf [Map.empty]

-- helpers
retFold (ss', rets) expr = do
  (ty, ss') <- typeOf ss' expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return (ss', updatedRets)

throwTE = throwError . GenericTE
-- throw if not equal
tine ty1 ty2 ss msg = if ty1 == ty2
                      then return (ty1, ss)
                      else throwTE msg

typeOfIdentifier ss ident@(PTerm (PIdentifier ide)) =
  if null ss
  then Nothing
  else case Map.lookup ide (topFrame ss) of
    Just ty -> return ty
    Nothing -> typeOfIdentifier (popFrame ss) ident

-- check the type of an expression
typeOf :: SymStack -> Expr -> TypeCheckMonad (Type, SymStack)

-- base cases
typeOf ss (PTerm (PInteger _)) = return (PIntegerType, ss)
typeOf ss (PTerm (PFloat _)) = return (PFloatType, ss)
typeOf ss (PTerm (PString _)) = return (PStringType, ss)
typeOf ss (PTerm (PBool _)) = return (PBoolType, ss)

-- compound types
typeOf ss (PFunc (PFuncType args retType) body) = do
  let argTypes = map (\(PTypeArg ty _) -> ty) args
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) args
  let ss' = pushFrame ss frame
  (bodyRetType, _) <- typeOf ss' body
  when (bodyRetType /= retType) (throwTE (printf "Return type %s does not match annotated type %s"
                                          (show bodyRetType) (show retType)))
  -- when (bodyRetType /= retType) (throwTE (printf "return type %s does not match annotated type %s" bodyRetType retType))
  return (PFuncType argTypes retType, ss)

typeOf ss (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf ss lhs
  (tyRhs, _) <- typeOf ss rhs
  let tyRet = if op `elem` boolOps then PBoolType else tyRhs
  if tyLhs == tyRhs
    then return (tyRet, ss)
    else throwTE $ printf "Binary operand types do not match (%s %s %s)"
         (show tyLhs) op (show tyRhs)

typeOf ss (PList (x:xs)) = do
  (tyHead, _) <- typeOf ss x
  let f acc i = typeOf ss i >>= (\(t, _) -> return $ acc && t == tyHead)
  valid <- foldM f True xs
  if valid
    then return (PListType tyHead, ss)
    else throwTE $ printf "Varying types in list (head is %s)" (show tyHead)

typeOf ss (PIf cond conseq alt) = do
  (tyCond, _) <- typeOf ss cond
  if (not . (==PBoolType)) tyCond
    then throwTE ("Expected if-condition to be of type Bool, got " ++ show tyCond)
    else do
      let ss' = pushFrame ss Map.empty
      (tyConseq, ss') <- typeOf ss conseq
      (tyAlt, ss') <- typeOf ss alt
      tine tyConseq tyAlt ss "Consequence and alternative in if-expression do not match"

typeOf ss (PAssign (PIdentifier ide) func@(PFunc tyFunc@(PFuncType params retType) body)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params
  let ss' = insertIde ss ide (PFuncType tyParams retType)
  typeOf ss' func
  return (tyFunc, ss')

typeOf ss (PAssign ident@(PIdentifier ide) val) = do
  (tyVal, _) <- typeOf ss val
  case typeOfIdentifier ss (PTerm ident) of
    Just existingTy -> if existingTy /= tyVal
                       then throwTE (printf "Reassigning variable '%s' of type %s with type %s"
                                     ide (show existingTy) (show tyVal))
                       else return (tyVal, insertIde ss ide tyVal) -- UPDATE
    Nothing -> return (tyVal, insertIde ss ide tyVal)

typeOf ss (PIndex arr idx) = do
  (tyArr, _) <- typeOf ss arr
  (tyIdx, _) <- typeOf ss idx
  case tyArr of
    PListType itemTy -> if PIntegerType /= tyIdx
                        then throwTE $ "Invalid index type, expected Int, got " ++ show tyIdx
                        else return (itemTy, ss)
    _ -> throwTE "The index operator is only defined for List # Int"

typeOf ss (PGroup body) = typeOf ss body

typeOf ss (PBlock StandardBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  --let valid = foldl (\acc ty -> acc && ty == head rets) True rets
  return (PBoolType, ssLast)

typeOf ss (PBlock FuncBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  when (null rets) (throwTE "Missing return statement")
  let valid = foldl (\acc ty -> acc && ty == head rets) True rets
  unless valid (throwTE "Varying return types in block")
  return (head rets, ssLast)

typeOf ss (PImmCall (PFunc (PFuncType params retType) body) args) = do
  let arityCheck cond msg = when (cond (length params) (length args)) $ throwTE $ printf "Too %s arguments given, expected %d, got %d" msg (length params) (length args)
  arityCheck (>) "few"
  arityCheck (<) "many"
  valid <- foldM (\acc (arg, param) -> do
                     let (PTypeArg tyParam _) = param
                     (tyArg, _) <- typeOf ss arg
                     return $ acc && tyArg == tyParam
                 ) True (zip args params)
  if valid
   then return (retType, ss)
   else throwTE "Wrong type of argument(s)"

typeOf ss (PCall ide args) = do
  (tyFunc, _) <- typeOf ss (PTerm ide)
  let (PFuncType funcArgs _) = tyFunc
  let arityCheck cond msg = when (cond (length funcArgs) (length args)) $ throwTE $ printf "too %s arguments given, expected %d, got %d" msg (length funcArgs) (length args)
  arityCheck (>) "few"
  arityCheck (<) "many"
  case tyFunc of
    PFuncType params retTy -> do
      valid <- foldM (\acc (arg, tyParam) -> do
                         (tyArg, _) <- typeOf ss arg
                         return $ acc && tyArg == tyParam
                     ) True (zip args params)
      if valid
        then return (retTy, ss)
        else throwTE "Wrong type of argument(s)"
    _ -> throwTE $ printf "Variable %s is not a function" (show ide)

typeOf ss ident@(PTerm (PIdentifier ide)) =
  case typeOfIdentifier ss ident of
    Just ty -> return (ty, ss)
    _ -> throwTE $ printf "reference to undefined variable %s" ide

typeOf ss (PReturn expr) = typeOf ss expr

-- catch all
typeOf _ _ = throwError $ GenericTE "Unknown type error"
