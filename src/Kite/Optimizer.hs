module Kite.Optimizer where

import Data.List

import Control.Monad.State
import Control.Monad.Error

import Kite.Syntax

data OptiError = OptiError String
               | UnknownError
               deriving (Show, Eq)

instance Error OptiError where
  noMsg = UnknownError
  strMsg = OptiError

throwOE :: String -> Opti a
throwOE = throwError . OptiError

type Name = String

data Needs = Needs { fns :: [String] }

data OptiState = OptiState { decls :: [Decl], scope :: [[Name]], checked :: [Name] }
type Opti a = ErrorT OptiError (State OptiState) a

builtInNames :: [Name]
builtInNames = ["print", "put", "sin", "cos", "tan", "sleep", "panic", "clear",
                "arguments", ":", "==", "<=", "+", "-", "*", "/", "%", "^", "random"]

builtIns :: [Decl]
builtIns = map (`PDecl` PVoid) builtInNames

findDecl :: Name -> Opti (Maybe Expr)
findDecl name = do
  st <- get
  let decl = find (\d -> case d of
                      PDecl dname _ -> dname == name
                      _ -> False
                  ) (decls st)
  case decl of
    Just (PDecl _ expr) -> return (Just expr)
    _ -> return Nothing

optimize :: [Decl] -> [Decl]
optimize ds = do
  let used = evalState (runErrorT optimize') OptiState { decls = ds ++ builtIns,
                                                         scope = [],
                                                         checked = [] }
  -- we expect this to always be Right, maybe refactor?
  let Right names = used
      keep = "main" : (builtInNames ++ names)

  filter (\d -> case d of
             PDecl name _ -> name `elem` keep
             _ -> False) ds

optimize' :: Opti [Name]
optimize' = do
  main <- findDecl "main"
  case main of
    Just expr -> mread expr
    Nothing -> throwOE "Can't optimize, no 'main' found"

mread' :: [Expr] -> Opti [Name]
mread' exprs = do
  a <- mapM mread exprs
  return $ concat a

mread :: Expr -> Opti [Name]

-- primitives
mread PVoid         = return []
mread (PInteger _)  = return []
mread (PFloat _)    = return []
mread (PChar _)     = return []
mread (PBool _)     = return []

-- base
mread (PIdentifier name) = do
  st <- get
  if name `elem` checked st
    then return [name]
    else do
    d <- findDecl name

    sub <- case d of
      Just expr -> do
        put st { checked = name : checked st }
        mread expr
      Nothing -> return []

    return $ name : sub

  -- look in scope, then decls, mread recursively
mread (PApply fn arg) = mread' [fn, arg]
mread (PList exprs) = mread' exprs
mread (PBlock exprs) = mread' exprs

mread (PPair exprA exprB) = mread' [exprA, exprB]
mread (PIf cond conseq alt) = mread' [cond, conseq, alt]
mread (PBind name expr) = do
  e <- mread expr
  return (e \\ [name])

mread (PLambda _ expr) = mread expr -- push scope
mread (PReturn expr) = mread expr
mread (PMatch expr cases) = mread' (expr : map snd cases)
