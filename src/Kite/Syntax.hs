module Kite.Syntax where

import Text.Printf
import Data.List
import Data.Char

data Pattern = PatCons String String
             | PatPair String String
             | PatPrimitive Expr
             | PatOtherwise
             deriving (Show, Eq)

type PatternCase = (Pattern, Expr)

data Draw = PDraw String Expr
            deriving(Show, Eq) -- Perhaps?

data Decl = PDecl String Expr
            deriving (Show, Eq)

data Expr = PList [Expr]
          | PBlock [Expr]
          | PPair Expr Expr
          | PIf Expr Expr Expr
          | PBind String Expr
          | PLambda String Expr
          | PApply Expr Expr
          | PReturn Expr
          | PMatch Expr [PatternCase]

          | PInteger Int
          | PFloat Float
          | PBool Bool
          | PChar Char
          | PIdentifier String
          | PVoid
          deriving (Show, Eq)

data Type = PListType Type
          | PPairType Type Type
          | PLambdaType Type Type
          | PBoolType
          | PIntegerType
          | PFloatType
          | PCharType
          | PTypeArg Type Expr -- PIdentifier!
          | PTypeVar String
          | PVoidType
          deriving (Eq)

--- Pretty printing

-- free types in nodes
free f@(PTypeVar _) = [f]

free PBoolType    = []
free PIntegerType = []
free PVoidType    = []
free PFloatType   = []
free PCharType    = []

free (PListType t)         = free t
free (PPairType ta tb)     = free ta `union` free tb
free (PLambdaType param ret) = free param `union` free ret
free (PTypeArg t _)        = free t

prettyType tmap t@(PTypeVar ide) =
  case find ((==t) . fst) tmap of
    Just a -> snd a
    Nothing -> ide

prettyType _ PBoolType    = "Bool"
prettyType _ PIntegerType = "Int"
prettyType _ PVoidType    = "Void"
prettyType _ PFloatType   = "Float"
prettyType _ PCharType    = "Char"

prettyType tmap (PPairType ta tb) =
  printf "(%s, %s)" (prettyType tmap ta) (prettyType tmap tb)

prettyType tmap (PListType t) =
  printf "[%s]" $ prettyType tmap t

prettyType tmap (PLambdaType tp@(PLambdaType _ _) tr) =
  printf "(%s) -> %s" (prettyType tmap tp) (prettyType tmap tr)

prettyType tmap (PLambdaType tp tr) =
  printf "%s -> %s" (prettyType tmap tp) (prettyType tmap tr)

prettyType _ (PTypeArg t ide) =
  printf "%s: %s" (show ide) (show t)

instance Show Type where
  -- compound types
  show lt@(PListType _) =
    let frees = nub (free lt)
        tmap = foldl (\acc t -> (t, [chr $ length acc + 97]) : acc) [] frees
    in prettyType tmap lt

  show ft@(PLambdaType _ _) =
    let frees = nub (free ft)
        tmap = foldl (\acc t -> (t, [chr $ length acc + 97]) : acc) [] frees
    in prettyType tmap ft

  -- monotypes
  show t = prettyType [] t

prettyDecls :: [Decl] -> String
prettyDecls decls = intercalate "\n\n" (map prettyDecl decls)

prettyDecl :: Decl -> String
prettyDecl (PDecl name expr) = printf "%s = %s" name (prettyExpr expr)

prettyExpr :: Expr -> String
prettyExpr (PInteger val) = show val
prettyExpr (PFloat val) = show val
prettyExpr (PBool val) = show val
prettyExpr (PChar val) = show val
prettyExpr (PIdentifier ide) = ide
prettyExpr PVoid = "Void"

prettyExpr (PList exprs) = "[" ++ intercalate "," (map prettyExpr exprs) ++ "]"
prettyExpr (PBlock exprs) = intercalate ";\n" (map prettyExpr exprs)
prettyExpr (PPair exprA exprB) = printf "(%s, %s)" (prettyExpr exprA) (prettyExpr exprB)
prettyExpr (PIf cond conseq alt) = printf "if %s\nthen %s\nelse %s" (prettyExpr cond) (prettyExpr conseq) (prettyExpr alt)
prettyExpr (PBind name expr) = printf "%s = %s" name (prettyExpr expr)
prettyExpr (PLambda ide expr) = "|" ++ ide ++ "| -> {\n" ++  (prettyExpr expr) ++ "\n}"
prettyExpr (PApply fn arg) = printf "(%s) (%s)" (prettyExpr fn) (prettyExpr arg)
prettyExpr (PReturn expr) = "return " ++ prettyExpr expr
prettyExpr (PMatch expr cases) = "match"
