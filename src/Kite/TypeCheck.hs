module Kite.TypeCheck where

import Kite.Parser
import Control.Monad.Error
import qualified Data.Map as Map

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

insertIde (x:xs) ide ty = (Map.insert ide ty x):xs

-- shortcuts
throwTE = throwError . GenericTE
-- throw if not equal
tine ty1 ty2 ss msg = if ty1 == ty2
                      then return (ty1, ss)
                      else throwTE msg

typeCheck :: Expr -> TypeCheckMonad (Type, SymStack)
typeCheck expr = typeOf [Map.empty] expr

-- check the type of an expression
typeOf :: SymStack -> Expr -> TypeCheckMonad (Type, SymStack)

-- base cases
typeOf ss (PTerm (PInteger _)) = return (PIntegerType, ss)
typeOf ss (PTerm (PFloat _)) = return (PFloatType, ss)
typeOf ss (PTerm (PString _)) = return (PStringType, ss)
typeOf ss (PTerm (PBool _)) = return (PBoolType, ss)

-- compound types
typeOf ss (PFunc (PFuncType args retTy) body) = do
  let ar = map (\(PTypeArg ty _) -> ty) args

  return $ (PFuncType ar retTy, ss)

typeOf ss (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf ss lhs
  (tyRhs, _) <- typeOf ss rhs
  tine tyRhs tyLhs ss "binary operand types do not match"

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
      (tyConseq, ss) <- typeOf ss conseq
      (tyAlt, ss) <- typeOf ss alt
      tine tyConseq tyAlt ss "consequence and alternative in if-expression do not match"

typeOf ss (PAssign ty (PIdentifier ide) val) = do
  let newSS = insertIde ss ide ty
  (tyVal, ss) <- typeOf newSS val
  tine ty tyVal newSS "type mismatch during assignment"

typeOf ss (PIndex arr idx) = do
  (tyArr, _) <- typeOf ss arr
  (tyIdx, _) <- typeOf ss idx
  case tyArr of
    PListType _ -> tine PIntegerType tyIdx ss ("invalid index type, expected Int, saw " ++ show tyIdx)
    _ -> throwTE "the index operator is only defined for \"<List>ls # <Int>idx\""

typeOf ss (PGroup body) = typeOf ss body

typeOf ss (PBlock exprs) =
  let f ss' expr = typeOf ss' expr >>= (\(_, ss') -> return ss')
  in foldM f ss exprs >>= \ssLast -> return $ (PBoolType, ssLast)

-- look up ide in symbol table, check arg types, then return type
-- typeOf PCall ide (x:xs) =

-- use symbol table
typeOf ss (PTerm (PIdentifier ide)) =
  case Map.lookup ide (topFrame ss) of
    Just ty -> return (ty, ss)
    Nothing -> throwTE ("reference to undefined variable " ++ ide)

-- catch all
typeOf _ _ = throwError $ GenericTE "Unknown type error"
