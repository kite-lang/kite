module Kite.TypeCheck where

import Kite.Parser
import Control.Monad.Error
import qualified Data.Map as Map

type Frame = Map.Map Term Type
type SymbolTable = [[Frame]]

data TypeError = GenericTE String
               | UnkownTE
               deriving (Show)

instance Error TypeError where
  noMsg = GenericTE "Unknown type error"
  strMsg = GenericTE

type TypeErrorMonad = Either TypeError

throwTE = throwError . GenericTE

-- throw if not equal
tine ty1 ty2 msg = if ty1 == ty2 then return ty1 else throwTE msg

-- check the type of an expression
typeOf :: Expr -> TypeErrorMonad Type

-- base cases
typeOf (PTerm (PInteger _)) = return PIntegerType
typeOf (PTerm (PFloat _)) = return PFloatType
typeOf (PTerm (PString _)) = return PStringType
typeOf (PTerm (PBool _)) = return PBoolType

-- compound types
typeOf (PFunc (PFuncType args retTy) _) = do
  let ar = map (\(PTypeArg ty _) -> ty) args
  return $ PFuncType ar retTy
  
typeOf (PBinOp op lhs rhs) = do
  tyLhs <- typeOf lhs
  tyRhs <- typeOf rhs
  tine tyRhs tyLhs "binary operand types do not match"

typeOf (PList (x:xs)) = do
  tyHead <- typeOf x
  let f acc i = typeOf i >>= (\t -> return $ acc && t == tyHead)
  valid <- foldM f True xs
  if valid
    then return (PListType tyHead)
    else throwTE "varying types in list"

typeOf (PIf cond conseq alt) = do
  tyCond <- typeOf cond
  if (not . (==PBoolType)) tyCond
    then throwTE ("expected if-condition to be of type Bool, saw " ++ show tyCond)
    else do
      tyConseq <- typeOf conseq
      tyAlt <- typeOf alt
      tine tyConseq tyAlt "consequence and alternative in if-expression do not match"

typeOf (PAssign ty ide val) = do
  -- identifier of type (typeOf val) to symtable
  tyVal <- typeOf val
  tine ty tyVal "type mismatch during assignment"

typeOf (PIndex arr idx) = do
  tyArr <- typeOf arr
  tyIdx <- typeOf idx
  case tyArr of
    PListType _ -> tine PIntegerType tyIdx ("invalid index type, expected Int, saw " ++ show tyIdx)
    _ -> throwTE "the index operator is only defined for \"<List>ls # <Int>idx\""

typeOf (PGroup body) = typeOf body
    
-- check return type
-- typeOf PFunc ty (PBlock (x:xs)) =

-- does these make any sense to check?
-- typeOf PReturn val =

-- look up ide in symbol table
-- typeOf PCall ide (x:xs) =
  
-- use symbol table
-- typeOf PTerm (PIdentifier _) = PIntegerType

-- catch all
typeOf _ = throwError $ GenericTE "Unknown type error"
