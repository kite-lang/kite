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

pushFrame stack frame = frame:stack
popFrame (x:xs) = xs
topFrame (x:xs) = x

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
  when (bodyRetType /= retType) (throwTE "return type does not match annotated one")
  return (PFuncType argTypes retType, ss)

typeOf ss (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf ss lhs
  (tyRhs, _) <- typeOf ss rhs
  let retTy = if op `elem` boolOps then PBoolType else tyRhs
  if tyLhs == tyRhs
    then return (retTy, ss)
    else throwTE "binary operand types do not match"

typeOf ss (PList (x:xs)) = do
  (tyHead, _) <- typeOf ss x
  let f acc i = typeOf ss i >>= (\(t, _) -> return $ acc && t == tyHead)
  valid <- foldM f True xs
  if valid
    then return (PListType tyHead, ss)
    else throwTE "varying types in list"

typeOf ss (PIf cond conseq alt) = do
  (tyCond, _) <- typeOf ss cond
  if (not . (==PBoolType)) tyCond
    then throwTE ("expected if-condition to be of type Bool, saw " ++ show tyCond)
    else do
      let ss' = pushFrame ss Map.empty
      (tyConseq, ss') <- typeOf ss conseq
      (tyAlt, ss') <- typeOf ss alt
      tine tyConseq tyAlt ss "consequence and alternative in if-expression do not match"

typeOf ss (PAssign (PIdentifier ide) val) = do
  (tyVal, _) <- typeOf ss val
  let newSS = insertIde ss ide tyVal
  return (tyVal, newSS)

typeOf ss (PIndex arr idx) = do
  (tyArr, _) <- typeOf ss arr
  (tyIdx, _) <- typeOf ss idx
  case tyArr of
    PListType itemTy -> if PIntegerType /= tyIdx
                     then throwTE ("invalid index type, expected Int, saw " ++ show tyIdx)
                     else return (itemTy, ss)
    _ -> throwTE "the index operator is only defined for List # Int"

typeOf ss (PGroup body) = typeOf ss body

typeOf ss (PBlock StandardBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  --let valid = foldl (\acc ty -> acc && ty == head rets) True rets
  return (PBoolType, ssLast)

-- DRY this up with above
typeOf ss (PBlock FuncBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  when (null rets) (throwTE "missing return statement")
  let valid = foldl (\acc ty -> acc && ty == head rets) True rets
  unless valid (throwTE "return types do not match")
  return (head rets, ssLast)

typeOf ss (PImmCall (PFunc (PFuncType params retType) body) args) = do
  let arityCheck cond msg = when (cond (length params) (length args)) $ throwTE $ printf "too %s arguments given, expected %d, got %d" msg (length params) (length args)
  arityCheck (>) "few"
  arityCheck (<) "many"
  valid <- foldM (\acc (arg, param) -> do
                     let (PTypeArg tyParam _) = param
                     (tyArg, _) <- typeOf ss arg
                     return $ acc && tyArg == tyParam
                 ) True (zip args params)
  if valid
   then return (retType, ss)
   else throwTE "wrong type of argument(s)"

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
        else throwTE "wrong type of argument(s)"
    _ -> throwTE (show ide ++ " is not a function")

typeOf ss ident@(PTerm (PIdentifier ide)) =
  if null ss
     then throwTE ("reference to undefined variable " ++ ide)
  else case Map.lookup ide (topFrame ss) of
    Just ty -> return (ty, ss)
    Nothing -> typeOf (popFrame ss) ident

typeOf ss (PReturn expr) = typeOf ss expr

-- catch all
typeOf _ _ = throwError $ GenericTE "Unknown type error"
