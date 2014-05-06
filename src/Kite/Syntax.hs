module Kite.Syntax where

import Text.Printf
import Data.List
import Data.Char

data BlockType = StandardBlock
               | FuncBlock
               deriving (Show, Eq)

data Pattern = PatCons String String
             | PatPair String String
             | PatPrimitive Expr
             | PatOtherwise
             deriving (Show, Eq)

type PatternCase = (Pattern, Expr)

data Expr = PList [Expr]
          | PBlock BlockType [Expr]
          | PPair Expr Expr
          | PIf Expr Expr Expr
          | PAssign Expr Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PCall Expr Expr -- PIdentifier!
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
          | PFuncType Type Type
          | PBoolType
          | PIntegerType
          | PFloatType
          | PCharType
          | PTypeArg Type Expr -- PIdentifier!
          | PFreeType String
          | PVoidType
          deriving (Eq)

--- Pretty printing

-- free types in nodes
free f@(PFreeType _) = [f]

free PBoolType    = []
free PIntegerType = []
free PVoidType    = []
free PFloatType   = []
free PCharType    = []

free (PListType t)         = free t
free (PPairType ta tb)     = free ta `union` free tb
free (PFuncType param ret) = free param `union` free ret
free (PTypeArg t _)        = free t

prettyType tmap t@(PFreeType ide) =
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

prettyType tmap (PFuncType tp@(PFuncType _ _) tr) =
  printf "(%s) -> %s" (prettyType tmap tp) (prettyType tmap tr)

prettyType tmap (PFuncType tp tr) =
  printf "%s -> %s" (prettyType tmap tp) (prettyType tmap tr)

prettyType _ (PTypeArg t ide) =
  printf "%s: %s" (show ide) (show t)

instance Show Type where
  -- compound types
  show lt@(PListType _) =
    let frees = nub (free lt)
        tmap = foldl (\acc t -> (t, [chr $ length acc + 97]) : acc) [] frees
    in prettyType tmap lt

  show ft@(PFuncType _ _) =
    let frees = nub (free ft)
        tmap = foldl (\acc t -> (t, [chr $ length acc + 97]) : acc) [] frees
    in prettyType tmap ft

  -- monotypes
  show t = prettyType [] t
