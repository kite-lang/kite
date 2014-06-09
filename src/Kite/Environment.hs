{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Kite.Environment where

import Debug.Trace
import Kite.Syntax

import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf

------------
-- Errors --
------------
data TypeError = TypeError String
               | ReferenceError String
               | ArityError String
               | UnknownError
               deriving (Show, Eq)

instance Error TypeError where
  noMsg = UnknownError
  strMsg = TypeError

printTrace :: [String] -> TC ()
printTrace stack = mapM_ (\m -> trace m $ return ()) (take 10 stack)

throwTE :: String -> TC a
throwTE = throwError . TypeError

throwRE :: String -> TC a
throwRE = throwError . ReferenceError

throwAE :: String -> Int -> Int -> TC a
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

-----------------------------
-- Environment state types --
-----------------------------
type Name = String
type Frame = Map.Map Name Type
type Stack = [Frame]

data Environment = Environment { sym :: Stack,
                                 types :: Frame,
                                 symCount :: Int,
                                 evalTrace :: [String],
                                 -- TODO: hack to infer pattern matches
                                 onlyFree :: Bool,
                                 returns :: [[Type]] }

instance Show Environment where
  show env =
    let syms = Map.toList $ head . sym $ env
        len = maximum (map (length . fst) syms)
        spaces = repeat ' '
        symstr = foldl (\acc (n, v) -> printf "%s%s%s%s\n" acc n (take (1 + len - length n) spaces) (show v)) "" syms
    in "Top symbol frame\n" ++ symstr

data Scheme = Scheme [String] Type
              deriving (Show)

newtype TypeEnvironment = TypeEnvironment (Map.Map String Scheme)
                        deriving (Show)

-- the monad in which all the state is kept and errors are thrown
type TC a = ErrorT TypeError (State Environment) a

runTC f = runState (runErrorT f) Environment { sym = [initSymbols],
                                               types = Map.empty,
                                               symCount = Map.size initSymbols,
                                               evalTrace = [],
                                               onlyFree = False,
                                               returns = [] }

------------------
-- Substitution --
------------------
type Substitution = Map.Map Name Type

nullSubst = Map.empty

composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1
(<+>) = composeSubst

------------------------------
-- Environment manipulation --
------------------------------
pushTrace x = do
  env <- get
  put env{evalTrace = x : evalTrace env}

pushReturnFrame = do
  env <- get
  put env{returns = [] : returns env}

popReturnFrame = do
  env <- get
  put env{returns = tail (returns env)}

popTrace = do
  env <- get
  put env{evalTrace = tail (evalTrace env)}

pushSymFrame = do
  env <- get
  put env{sym = Map.empty:sym env}

popSymFrame = do
  env <- get
  put env{sym = tail (sym env)}

------------------------
-- Built-in functions --
------------------------
mkConsSignature n = let t = PTypeVar ("t" ++ n) in PLambdaType t (PLambdaType (PListType t) (PListType t))
mkArithSignature n = let t = PTypeVar ("t" ++ n) in PLambdaType t (PLambdaType t t)
mkEqualitySignature n = let t = PTypeVar ("t" ++ n) in PLambdaType t (PLambdaType t PBoolType)

initSymbols =
  let arithOps = ["+", "-", "*", "/", "%", "^"]
      arithSigs = map (\(op, n) -> (op, mkArithSignature (show n))) (zip arithOps [0 .. length arithOps])
  in Map.fromList (arithSigs `union` [("<=", mkEqualitySignature "5"),
                                      ("==", mkEqualitySignature "6"),
                                      (":", mkConsSignature "7"),
                                      ("print", PLambdaType (PTypeVar "8") PVoidType),
                                      ("random", PLambdaType PIntegerType (PLambdaType PIntegerType PIntegerType) ),
                                      ("put", PLambdaType (PTypeVar "9") PVoidType),
                                      ("sin", PLambdaType PFloatType PFloatType),
                                      ("cos", PLambdaType PFloatType PFloatType),
                                      ("tan", PLambdaType PFloatType PFloatType),
                                      ("sqrt", PLambdaType PFloatType PFloatType),
                                      ("sleep", PLambdaType PIntegerType PVoidType),
                                      ("panic", PLambdaType (PListType PCharType) (PTypeVar "10")),
                                      ("clear", PLambdaType PVoidType PVoidType),
                                      ("arguments", PLambdaType PVoidType (PListType (PListType PCharType)))])

-----------------
-- Types class --
-----------------
class Types a where
  ftv :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types TypeEnvironment where
  ftv (TypeEnvironment env) = ftv (Map.elems env)
  apply s (TypeEnvironment env) = TypeEnvironment (Map.map (apply s) env)

instance Types Type where
  ftv (PTypeVar ide) = Set.singleton ide
  ftv (PListType t) = ftv t
  ftv (PPairType ta tb) = ftv ta `Set.union` ftv tb
  ftv (PLambdaType tParam tRet) = ftv tParam `Set.union` ftv tRet
  ftv _ = Set.empty
  apply s (PTypeVar ide) = fromMaybe (PTypeVar ide) (Map.lookup ide s)
  apply s (PLambdaType tParam tRet) = PLambdaType (apply s tParam) (apply s tRet)
  apply s (PListType t) = PListType (apply s t)
  apply s (PPairType a b) = PPairType (apply s a) (apply s b)
  apply s t = t

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
  apply s = map (apply s)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

-------------------------
-- Environment helpers --
-------------------------
remove :: TypeEnvironment -> String -> TypeEnvironment
remove (TypeEnvironment env) ide = TypeEnvironment (Map.delete ide env)

-- generalize :: TypeEnvironment -> Type -> Scheme
-- generalize env t = Scheme (Set.toList (ftv t Set.\\ ftv env)) t

instantiate :: Scheme -> TC Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (\_ -> freshTypeVar "t") vars
  let s = Map.fromList (zip vars freshVars)
  return (apply s t)

-- generate a fresh type variable
freshTypeVar :: String -> TC Type
freshTypeVar ide = do
  env <- get
  let count = symCount env
  put env{symCount =  succ count}
  return $ PTypeVar (ide ++ show count)

removeSym :: String -> TC ()
removeSym ide = do
  env <- get
  let (x:xs) = sym env
      newF = Map.delete ide x
  put env{sym = newF:xs}

insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}

insertType :: String -> Type -> TC ()
insertType ide val = do
  env <- get
  put env{types = Map.insert ide val (types env)}

lookupType :: String -> TC (Maybe Type)
lookupType ide = do
  env <- get
  return $ Map.lookup ide (types env)
