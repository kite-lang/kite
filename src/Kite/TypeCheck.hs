{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Kite.TypeCheck where

import Debug.Trace
import Kite.Parser

import Data.Maybe
import Control.Monad.State
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
type Frame = Map.Map String Type
type Stack = [Frame]
data Environment = Environment { sym :: Stack,
                                 ftv :: Stack}

instance Show Environment where
  show env = "SYMs: " ++ show (sym env) ++ "   FTVs: " ++ show (ftv env)

pushSymbolFrame env frame = env{sym = frame : sym env}
popSymbolFrame env = env{sym = tail (sym env)}
topSymbolFrame env = head (sym env)
insertSymbol env ide ty =
  trace (printf "insertSymbol %s:%s" ide (show ty)) $
  env{sym = Map.insert ide ty (topSymbolFrame env) : tail (sym env)}

pushTypeFrame env frame = env{ftv = frame : ftv env}
popTypeFrame env = env{ftv = tail (ftv env)}
topTypeFrame env = head (ftv env)
insertType env ide ty =
  trace (printf "insertType %s:%s" ide (show ty)) $
  env{ftv = Map.insert ide ty (topTypeFrame env) : tail (ftv env)}

symbolFrameCount env = length (sym env)
typeFrameCount env = length (ftv env)

freshTypeName :: Environment -> String -> String
freshTypeName env ide =
  let count = 5 -- sum $ map length (ftv env)
  in ide ++ show count

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


-- main interface
typeCheck :: Expr -> TypeCheckMonad (Type, Environment)
typeCheck = typeOf (Environment [Map.empty] [Map.empty])

-- helpers
extractReturnType (env, rets) expr = do
  (ty, env') <- typeOf env expr
  let updatedRets = case expr of
        PReturn _ -> ty : rets
        _ -> rets
  return (env', updatedRets)

throwTE = throwError . TypeError
throwRE = throwError . ReferenceError
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

-- throw if not equal
tine ty1 ty2 env msg = if ty1 == ty2
                      then return (ty1, env)
                      else throwTE msg

lookupSymbolType :: Environment -> Expr -> Maybe Type
lookupSymbolType env ident@(PIdentifier ide) =
  trace ("SYM lookup " ++ ide ++ show env) $
  if symbolFrameCount env == 0
  then Nothing
  else case Map.lookup ide (topSymbolFrame env) of
    Just free@(PFreeType ftvId) -> case lookupFtvType env ftvId of
      Just ty -> return ty
      Nothing -> return free
    Just ty -> return ty
    Nothing -> lookupSymbolType (popSymbolFrame env) ident

lookupFtvType env ide =
  trace ("FTV lookup " ++ ide ++ show env) $
  if typeFrameCount env == 0
  then Nothing
  else case Map.lookup ide (topTypeFrame env) of
    Just ty -> return ty
    Nothing -> lookupFtvType (popTypeFrame env) ide

-- instantiate a free type
instantiate :: Environment -> String -> Type -> Environment
instantiate = insertType

-- bind a free type to a concrete one
bindType env (PFreeType ide) concrete =
  env{ftv = Map.adjust (const concrete) ide (topTypeFrame env) : tail (ftv env)}

isFree t = case t of
  PFreeType _ -> True
  _ -> False

freeName t = case t of
  PFreeType name -> name
  _ -> ""

        -- PFreeType name -> let ta'' = fromMaybe ta (lookupFtvType env name)
        --                   in if not . isFree tb
        --                      then instantiate env
        --                      else

-- unify two types
unify :: Environment -> Type -> Type -> TypeCheckMonad (Type, Environment)

unify env (PFreeType na) tb =
  let env' = instantiate env na tb
  in return (tb, env')

unify env ta tb | ta == tb = return (ta, env)
                | isFree tb = unify env tb ta
                | otherwise = throwTE $ printf "Type mismatch (%s and %s)" (show ta) (show tb)

-- check the type of an expression
typeOf :: Environment -> Expr -> TypeCheckMonad (Type, Environment)

-- base cases
typeOf env (PInteger _) = return (PIntegerType, env)
typeOf env (PFloat _) = return (PFloatType, env)
typeOf env (PString _) = return (PStringType, env)
typeOf env (PBool _) = return (PBoolType, env)

-- compound types
typeOf env (PFunc (PFuncType args retType) body) = do
  let argTypes = map (\(PTypeArg ty _) -> ty) args
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) args
  -- TODO: give fresh names same free type identifiers
  let env' = pushSymbolFrame env frame
  (bodyRetType, env'') <- typeOf env' body
  
  -- add instantiated types from function body to current environment but ignore symbols
  let env2 = env'{ftv = ftv env''}
  trace (show env'') $ return ()
  let retType' = case retType of
        PFreeType ide -> fromMaybe retType (lookupFtvType env' ide)
        _ -> retType
  --bindType 
  when ((lookupFtvType bodyRetType) /= retType') $ throwTE $ printf
    "Return type %s does not match annotated type %s."
    (show bodyRetType) (show retType')
  return (PFuncType argTypes retType', env2)

typeOf env (PBinOp op lhs rhs) = do
  (tyLhs, _) <- typeOf env lhs
  (tyRhs, _) <- typeOf env rhs

  (unifiedType, env') <- unify env tyLhs tyRhs

  let retTy = if op `elem` boolOps then PBoolType else unifiedType
  unless ((retTy,op,retTy) `elem` binOpLookup ||
          retTy `elem` [PIntegerType,PFloatType] || -- All binary ops are allowed for Int and Float
          op `elem` boolOps) -- All boolean ops are allowed for all types
    $ throwTE $ printf "Binary operator '%s' is not allowed for types '%s' and '%s'."
    op (show tyRhs) (show tyLhs)
  -- TODO: get this back
  -- unless (tyLhs == tyRhs || (tyLhs,op,tyRhs) `elem` binOpLookup) $ throwTE $ printf
  --   "Binary operand types do not match (%s %s %s)."
  --   (show tyLhs) op (show tyRhs)
  return (retTy, env')

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
      let env' = pushSymbolFrame env Map.empty
      (tyConseq, _) <- typeOf env' conseq
      (tyAlt, _) <- typeOf env' alt
      tine tyConseq tyAlt env $ printf
        "Consequence and alternative in if-expression do not match (%s and %s)."
        (show tyConseq) (show tyAlt)

typeOf env (PAssign (PIdentifier ide) func@(PFunc tyFunc@(PFuncType params retType) _)) = do
  let tyParams = map (\(PTypeArg ty _) -> ty) params
  let env' = trace (printf "assign %s:%s" ide (show (PFuncType tyParams retType))) $ insertSymbol env ide (PFuncType tyParams retType)
  _ <- typeOf env' func
  return (tyFunc, env')

typeOf env (PAssign ident@(PIdentifier ide) val) = do
  (tyVal, _) <- typeOf env val
  case lookupSymbolType env ident of
    Just existingTy -> trace "FUUUUUUUUUCK" $ if existingTy /= tyVal
                       then trace "AWDAWDAWDAWDAW" $ throwTE $ printf
                            "Reassigning variable '%s' of type %s with type %s."
                            ide (show existingTy) (show tyVal)
                       else trace ("AWD " ++ (show $ sym(insertSymbol env ide tyVal))) $ return (tyVal, insertSymbol env ide tyVal) -- UPDATE
    Nothing -> return (tyVal, insertSymbol env ide tyVal)

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
  (envLast, rets) <- foldM extractReturnType (env, []) exprs
  --let tyRet = if null rets then PVoidType else head rets
  return (PBoolType, envLast)

typeOf env (PBlock FuncBlock exprs) = do
  (envLast, rets) <- foldM extractReturnType (env, []) exprs
  when (null rets) $ throwTE "Missing return statement."
  mapM_ (\ty ->
          when (ty /= head rets) $ throwTE "Varying return types in block."
        ) rets
  return (head rets, envLast)

typeOf env (PImmCall (PFunc (PFuncType params retType) body) args) = do
  when (length params /= length args) $ throwAE
    "<anonymous>" (length params) (length args)
  let frame = Map.fromList $ map (\(PTypeArg ty (PIdentifier ide)) -> (ide, ty)) params
  let env' = pushSymbolFrame env frame
  _ <- typeOf env' body
  mapM_ (\(arg, param) -> do
            let (PTypeArg tyParam _) = param
            (tyArg, _) <- typeOf env arg
            when (tyArg /= tyParam) $ throwTE $ printf
              "Wrong type of argument. Expected %s, got %s."
              (show tyParam) (show tyArg)
        ) (zip args params)
  return (retType, env)

typeOf env (PCall ident@(PIdentifier ide) args) = do
  (tyFunc, _) <- typeOf env ident
  let (PFuncType params retTy) = trace ("ABE " ++ show params) $ tyFunc
  when (length params /= length args) $ throwAE ide (length params) (length args)
  case tyFunc of
    PFuncType _ _ -> do
      mapM_ (\(arg, tyParam) -> do
                (tyArg, _) <- typeOf env arg
                unify env tyArg tyParam

                --when (tyArg /= tyParam) $ throwTE $ printf
                -- "Wrong type of argument when calling function '%s'. Expected %s, got %s."
                  --ide (show tyParam) (show tyArg)
            ) (zip args params)
      return (retTy, env)
    _ -> throwTE $ printf "Variable '%s' is not a function." (show ide)

typeOf env ident@(PIdentifier ide) =
  case lookupSymbolType env ident of
    Just ty -> return (ty, env)
    Nothing -> throwRE $ printf "Reference to undefined variable '%s'." ide

typeOf env (PReturn expr) = typeOf env expr

-- catch all
typeOf _ _ = throwError UnknownError
