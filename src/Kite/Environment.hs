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
               deriving (Eq)

instance Show TypeError where
  show (TypeError str) = "Type error: " ++ str
  show (ReferenceError str) = "Reference error: " ++ str
  show (ArityError str) = "Arity error: " ++ str
  show UnknownError = "Unknown error"

instance Error TypeError where
  noMsg = UnknownError
  strMsg = TypeError

-- | Prints a stack of traces to stderr.
printTrace :: [String] -> TC ()
printTrace stack = mapM_ (\m -> trace m $ return ()) (take 10 stack)

-- | Utility function for throwing a `TypeError`.
throwTE :: String -> TC a
throwTE = throwError . TypeError

-- | Utility function for throwing a `ReferenceError`.
throwRE :: String -> TC a
throwRE = throwError . ReferenceError

-- | Utility function for throwing an `ArityError`.
throwAE :: String -> Int -> Int -> TC a
throwAE ide exp got = throwError . ArityError $ printf
                      "Function '%s' got too %s arguments. Expected %d, got %d."
                      ide (if exp > got then "few" else "many") exp got

-----------------------------
-- Environment state types --
-----------------------------
-- | Some type synonyms used to increase readability throughout
-- type-checking.
type Name = String
type Frame = Map.Map Name Type
type Stack = [Frame]

-- | The environment containing the state of the type-checker as it
-- analyses the AST.
data Environment = Environment { sym :: Stack,
                                 types :: Frame,
                                 typeAlias :: Frame,
                                 symCount :: Int,
                                 evalTrace :: [String],
                                 -- TODO: hack to infer pattern matches
                                 onlyFree :: Bool,
                                 returns :: [[Type]] }

-- | A Show instance of Environment making the environment readable
-- for debugging.
instance Show Environment where
  show env =
    let syms = Map.toList $ head . sym $ env
        len = maximum (map (length . fst) syms)
        spaces = repeat ' '
        symstr = foldl (\acc (n, v) -> printf "%s%s%s%s\n" acc n (take (1 + len - length n) spaces) (show v)) "" syms
    in "Top symbol frame\n" ++ symstr

-- | A type scheme that represents a type and a set of type variables
-- that are bound in the type.
data Scheme = Scheme [String] Type
              deriving (Show)

-- | An environment representing used to keep track of the inferred
-- types of type variables. It is passed along in the `infer`
-- function.
newtype TypeEnvironment = TypeEnvironment (Map.Map Name Scheme)
                        deriving (Show)

-- | The monad in which all the state is kept and errors are thrown
-- while type-checking.
type TC a = ErrorT TypeError (State Environment) a

-- | Runs a function in the `TC` monad with an initial `Environment`
runTC f = runState (runErrorT f) Environment { sym = [initSymbols],
                                               types = Map.empty,
                                               typeAlias = Map.empty,
                                               symCount = Map.size initSymbols,
                                               evalTrace = [],
                                               onlyFree = False,
                                               returns = [] }

------------------
-- Substitution --
------------------
-- | A substitution of type variables to type terms. Computed in the
-- `unify`.
type Substitution = Map.Map Name Type

-- | Shorthand for an empty subsitution
nullSubst = Map.empty

-- | Composes (unions) two substitutions
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Infix alias of `composeSubst`
(<+>) = composeSubst

------------------------------
-- Environment manipulation --
------------------------------
-- | Methods used to manipulate the stack of the `Environment`
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

-- | The built-in and predefined functions with accompanying
-- types. These will be included in the initial `Environment` when
-- running the type-checker using `runTC`
initSymbols =
  let arithOps = ["+", "-", "*", "/", "%", "^"]
      arithSigs = map (\(op, n) -> (op, mkArithSignature (show n))) (zip arithOps [0 .. length arithOps])
  in Map.fromList (arithSigs `union` [("<=", mkEqualitySignature "5"),
                                      ("==", mkEqualitySignature "6"),
                                      (":", mkConsSignature "7"),
                                      ("print", PLambdaType (PTypeVar "8") PVoidType),
                                      ("show", PLambdaType (PTypeVar "9") (PListType PCharType)),
                                      ("random", PLambdaType PIntegerType (PLambdaType PIntegerType PIntegerType) ),
                                      ("put", PLambdaType (PTypeVar "10") PVoidType),
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
-- | The `Types` class is used to enable application of substitution
-- into types, lists of types, type schemes, etc with the `apply`
-- function. It also defines `ftv` which is used to extract all free
-- types in a type.
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

-- | Instantiate a type scheme
instantiate :: Scheme -> TC Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (\_ -> freshTypeVar "t") vars
  let s = Map.fromList (zip vars freshVars)
  return (apply s t)

-- | Generate a fresh type variable with a unique name
freshTypeVar :: String -> TC Type
freshTypeVar ide = do
  env <- get
  let count = symCount env
  put env{symCount =  succ count}
  return $ PTypeVar (ide ++ show count)

-- | Remove a symbol and its type from the `Environment`
removeSym :: String -> TC ()
removeSym ide = do
  env <- get
  let (x:xs) = sym env
      newF = Map.delete ide x
  put env{sym = newF:xs}

-- | Insert a symbol and its type into the `Environment`
insertSym :: String -> Type -> TC ()
insertSym ide val = do
  env <- get
  let (x:xs) = sym env
      newF = Map.insert ide val x
  put env{sym = newF:xs}

-- | Insert a type annotation with its accompanying identifier
insertType :: String -> Type -> TC ()
insertType ide val = do
  env <- get
  put env{types = Map.insert ide val (types env)}

-- | Lookup a type in the `Environment`
lookupType :: String -> TC (Maybe Type)
lookupType ide = do
  env <- get
  return $ Map.lookup ide (types env)

-- | Insert a type alias with its accompanying identifier
insertTypeAlias :: String -> Type -> TC ()
insertTypeAlias ide val = do
  env <- get
  put env{typeAlias = Map.insert ide val (typeAlias env)}

-- | Lookup a type alias in the `Environment`
lookupTypeAlias :: String -> TC (Maybe Type)
lookupTypeAlias ide = do
  env <- get
  return $ Map.lookup ide (typeAlias env)
