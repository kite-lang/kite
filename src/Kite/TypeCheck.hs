module Kite.TypeCheck (typeCheck) where

import Kite.Parser
import Control.Monad.Error
import qualified Data.Map as Map
import Text.Printf

-- error handling
data TypeError = TypeError String
               | ReferenceError String
               | ArityError String
               | UnknownError
               deriving (Show)

instance Error TypeError where
  noMsg = UnknownError
  strMsg = TypeError

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
typeCheck :: Expr -> TypeCheckMonad (Type, SymStack)
typeCheck = typeOf [Map.empty]

-- helpers
retFold (ss', rets) expr = do
  (ty, ss') <- typeOf ss' expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return (ss', updatedRets)

throwTE = throwError . TypeError
throwRE = throwError . ReferenceError
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

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
  when (bodyRetType /= retType) $ throwTE $ printf
    "Return type %s does not match annotated type %s."
    (show bodyRetType) (show retType)
  return (PFuncType argTypes retType, ss)

typeOf ss (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf ss lhs
  (tyRhs, _) <- typeOf ss rhs
  let retTy = if op `elem` boolOps then PBoolType else tyRhs
  if (retTy,op,retTy) `elem` binOpLookup || 
     retTy `elem` [PIntegerType,PFloatType] || -- All binary ops are allowed for Int and Float
     op `elem` boolOps -- All boolean ops are allowed for all types 
  then
      if tyLhs == tyRhs
      then return (retTy, ss)
      else if (tyLhs,op,tyRhs) `elem` binOpLookup 
           then return (retTy, ss)
           else throwTE $ printf "Binary operand types do not match (%s %s %s)"
               (show tyLhs) op (show tyRhs)
  else  throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'"
                op (show tyRhs) (show tyLhs)


typeOf ss (PList (x:xs)) = do
  (tyHead, _) <- typeOf ss x
  mapM_ (\i -> do
            (ty, _) <- typeOf ss i
            when (ty /= tyHead) $ throwTE $ printf
              "Varying types in list. Got %s(s) and %s(s)."
              (show tyHead) (show ty)
        ) xs
  return (PListType tyHead, ss)

typeOf ss (PIf cond conseq alt) = do
  (tyCond, _) <- typeOf ss cond
  if (not . (==PBoolType)) tyCond
    then throwTE $ printf
         "Expected if-condition to be of type Bool, got %s." (show tyCond)
    else do
      let ss' = pushFrame ss Map.empty
      (tyConseq, _) <- typeOf ss' conseq
      (tyAlt, _) <- typeOf ss' alt
      tine tyConseq tyAlt ss $ printf
        "Consequence and alternative in if-expression do not match (%s and %s)."
        (show tyConseq) (show tyAlt)

typeOf ss (PAssign (PIdentifier ide) func@(PFunc tyFunc@(PFuncType params retType) _)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params
  let ss' = insertIde ss ide (PFuncType tyParams retType)
  typeOf ss' func
  return (tyFunc, ss')

typeOf ss (PAssign ident@(PIdentifier ide) val) = do
  (tyVal, _) <- typeOf ss val
  case typeOfIdentifier ss (PTerm ident) of
    Just existingTy -> if existingTy /= tyVal
                       then throwTE $ printf
                            "Reassigning variable '%s' of type %s with type %s."
                            ide (show existingTy) (show tyVal)
                       else return (tyVal, insertIde ss ide tyVal) -- UPDATE
    Nothing -> return (tyVal, insertIde ss ide tyVal)

typeOf ss (PIndex arr idx) = do
  (tyArr, _) <- typeOf ss arr
  (tyIdx, _) <- typeOf ss idx
  case tyArr of
    PListType itemTy -> if PIntegerType /= tyIdx
                        then throwTE $ printf
                             "Invalid index type, expected Int, got %s."
                             (show tyIdx)
                        else return (itemTy, ss)
    _ -> throwTE "The index operator is only defined for List # Int."

typeOf ss (PGroup body) = typeOf ss body

typeOf ss (PBlock StandardBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  --let valid = foldl (\acc ty -> acc && ty == head rets) True rets
  return (PBoolType, ssLast)

typeOf ss (PBlock FuncBlock exprs) = do
  (ssLast, rets) <- foldM retFold (ss, []) exprs
  when (null rets) $ throwTE "Missing return statement."
  mapM_ (\ty ->
          when (ty /= head rets) $ throwTE "Varying return types in block."
        ) rets
  return (head rets, ssLast)

typeOf ss (PImmCall (PFunc (PFuncType params retType) body) args) = do
  when (length params /= length args) $ throwAE
    "<anonymous>" (length params) (length args)
  mapM_ (\(arg, param) -> do
            let (PTypeArg tyParam _) = param
            (tyArg, _) <- typeOf ss arg
            when (tyArg /= tyParam) $ throwTE $ printf
              "Wrong type of argument. Expected %s, got %s."
              (show tyParam) (show tyArg)
        ) (zip args params)
  return (retType, ss)

typeOf ss (PCall ident@(PIdentifier ide) args) = do
  (tyFunc, _) <- typeOf ss (PTerm ident)
  let (PFuncType params retTy) = tyFunc
  when (length params /= length args) $ throwAE ide (length params) (length args)
  case tyFunc of
    PFuncType _ _ -> do
      mapM_ (\(arg, tyParam) -> do
                (tyArg, _) <- typeOf ss arg
                when (tyArg /= tyParam) $ throwTE $ printf
                  "Wrong type of argument when calling function '%s'. Expected %s, got %s."
                  ide (show tyParam) (show tyArg)
            ) (zip args params)
      return (retTy, ss)
    _ -> throwTE $ printf "Variable '%s' is not a function." (show ide)

typeOf ss ident@(PTerm (PIdentifier ide)) =
  case typeOfIdentifier ss ident of
    Just ty -> return (ty, ss)
    _ -> throwRE $ printf "Reference to undefined variable '%s'." ide

typeOf ss (PReturn expr) = typeOf ss expr

-- catch all
typeOf _ _ = throwError UnknownError
