module Kite.Syntax where

import Text.Printf
import Data.List
import Data.Char

data BlockType = StandardBlock
               | FuncBlock
               deriving (Show, Eq)

data Expr = PList [Expr]
          | PBlock BlockType [Expr]
          | PTuple [Expr]
          | PIf Expr Expr Expr
          | PAssign Expr Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PCall Expr Expr -- PIdentifier!
          | PReturn Expr

          | PInteger Int
          | PFloat Float
          | PBool Bool
          | PChar Char
          | PIdentifier String
          | PVoid
          deriving (Show, Eq)

data Type = PListType Type
          | PTupleType [Type]
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
free (PFuncType param ret) = free param `union` free ret
free (PTypeArg t _)        = free t

isFree (PFreeType _) = True
isFree _ = False

prettyType tmap t@(PFreeType _) =
  case find ((==t) . fst) tmap of
    Just a -> snd a
    Nothing -> error "Unbound type"

prettyType _ PBoolType    = "Bool"
prettyType _ PIntegerType = "Int"
prettyType _ PVoidType    = "Void"
prettyType _ PFloatType   = "Float"
prettyType _ PCharType    = "Char"

prettyType tmap (PListType t)     = printf "[%s]" $ prettyType tmap t
prettyType tmap (PFuncType tp tr) = printf "(%s -> %s)" (prettyType tmap tp) (prettyType tmap tr)
prettyType _ (PTypeArg t ide)     = printf "%s: %s" (show ide) (show t)

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
  show t             = prettyType [] t
