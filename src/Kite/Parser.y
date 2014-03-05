{
module Kite.Parser where
import Kite.Lexer
import Text.Printf
}

%name kiteparser
%tokentype { Token }

%token
        int                { Integer _ $$ }
        float              { Float _ $$ }
        string             { String _ $$ }
        bool               { Bool _ $$ }

        intTy              { Type _ "Int" }
        floatTy            { Type _ "Float" }
        stringTy           { Type _ "String" }
        boolTy             { Type _ "Bool" }
        id                 { Identifier _ $$ }

        '+'                { BinOp _ "+" }
        '-'                { BinOp _ "-" }
        '*'                { BinOp _ "*" }
        '/'                { BinOp _ "/" }
        '%'                { BinOp _ "%" }
        '=='               { BinOp _ "==" }
        '<'                { BinOp _ "<" }
        '<='               { BinOp _ "<=" }
        '>'                { BinOp _ ">" }
        '>='               { BinOp _ ">=" }
        '!='               { BinOp _ "!=" }

        return             { Keyword _ "return" }
        if                 { Keyword _ "if" }
        then               { Keyword _ "then" }
        else               { Keyword _ "else" }
        yolo               { Keyword _ "yolo" }

        '='                { Operator _ "=" }
        '#'                { Operator _ "#" }
        '->'               { Operator _ "->" }

        '('                { Symbol _ '(' }
        ')'                { Symbol _ ')' }
        '['                { Symbol _ '[' }
        ']'                { Symbol _ ']' }
        '{'                { Symbol _ '{' }
        '}'                { Symbol _ '}' }
        ','                { Symbol _ ',' }
        ';'                { Symbol _ ';' }

%right in
%nonassoc '==' '<' '<=' '>' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

%%

Stmt    : Expr             { $1 }
        | StandardBlock    { $1 }
        | return Expr      { PReturn $2 }

Stmts   : {- nothing -}    { [] }
        | Stmt             { [$1] }
        | Stmt Stmts       { $1 : $2 }
        | Stmt ';' Stmts   { $1 : $3 }

Expr    : BinOp            { $1 }
        | List             { $1 }
        | Assign           { $1 }
        | Func             { $1 }
        | Call             { $1 }
        | If               { $1 }
        | Index            { $1 }
        | Term             { PTerm $1 }
        | '(' Expr ')'     { PGroup $2 }

Exprs   : {- nothing -}    { [] }
        | Expr             { [$1] }
        | Expr ',' Exprs   { $1 : $3 }

-- expression rules
Call    : id '(' Exprs ')'    { PCall (PIdentifier $1) $3 }
        | Func '(' Exprs ')'  { PImmCall $1 $3 }

Index   : Expr '#' Expr     { PIndex $1 $3 }

Assign  : id '=' Expr      { PAssign (PIdentifier $1) $3 }

-- differentiate between standard and function blocks
StandardBlock : '{' Stmts '}'    { PBlock StandardBlock $2 }

FuncBlock : '{' Stmts '}'    { PBlock FuncBlock $2 }

List    : '[' Exprs ']'    { PList $2 }

BinOp   : Expr '+' Expr  { PBinOp "+" $1 $3 }
        | Expr '-' Expr  { PBinOp "-" $1 $3 }
        | Expr '*' Expr  { PBinOp "*" $1 $3 }
        | Expr '/' Expr  { PBinOp "/" $1 $3 }
        | Expr '%' Expr  { PBinOp "%" $1 $3 }
        | Expr '==' Expr { PBinOp "==" $1 $3 }
        | Expr '<' Expr  { PBinOp "<" $1 $3 }
        | Expr '<=' Expr { PBinOp "<=" $1 $3 }
        | Expr '>' Expr  { PBinOp ">" $1 $3 }
        | Expr '>=' Expr { PBinOp ">=" $1 $3 }
        | Expr '!=' Expr { PBinOp "!=" $1 $3 }


Func    : FuncDef FuncBlock    { PFunc $1 $2 }

Type    : boolTy           { PBoolType }
        | intTy            { PIntegerType }
        | floatTy          { PFloatType }
        | stringTy         { PStringType }
        | '[' Type ']'     { PListType $2 }
        | FuncType         { $1 }

TypeArg : Type id          { PTypeArg $1 (PIdentifier $2) }

-- support both single expr and blocks
If    : if Expr then Expr else Expr    { PIf $2 $4 $6 }
      | if Expr then StandardBlock else StandardBlock  { PIf $2 $4 $6 }

-- func literal
FuncDef : '(' ArgList ')' '->' Type { PFuncType $2 $5 }

-- named arguments
ArgList : {- nothing -}    { [] }
        | TypeArg             { [$1] }
        | TypeArg ',' ArgList   { $1 : $3 }

-- func signature
FuncType : '(' TypeList ')' '->' Type { PFuncType $2 $5 }

-- just type
TypeList: {- nothing -}    { [] }
        | Type             { [$1] }
        | Type ',' TypeList   { $1 : $3 }

-- primitive types
Term    : int              { PInteger $1 }
        | bool             { PBool $1 }
        | float            { PFloat $1 }
        | string           { PString $1 }
        | id               { PIdentifier $1 }

{

-- error handling
happyError (x:xs) = error $ "Parse error: " ++ (show . posn2str . tok2posn) x

posn2str (AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col

data BlockType = StandardBlock
               | FuncBlock
               deriving (Show, Eq)

data Expr = PBinOp String Expr Expr -- Operator!
          | PTerm Term
          | PList [Expr]
          | PBlock BlockType [Expr]
          | PIf Expr Expr Expr
          | PAssign Term Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PGroup Expr -- PFuncType!
          | PCall Term [Expr] -- PIdentifier!
          | PImmCall Expr [Expr] -- PIdentifier!
          | PReturn Expr
          | PIndex Expr Expr
          deriving (Show, Eq)

data Type = PListType Type
          | PFuncType [Type] Type
          | PBoolType
          | PIntegerType
          | PFloatType
          | PStringType
          | PTypeArg Type Term -- PIdentifier!
          deriving (Eq)

data Term = PInteger Int
          | PFloat Float
          | PBool Bool
          | PString String
          | PIdentifier String
          deriving (Show, Eq)

instance Show Type where
  show (PListType ty)        = printf "List %s" $ show ty
  show (PFuncType [] ty)     = printf "Func [] %s" $ show ty
  show (PFuncType (x:xs) ty) = printf "Func [%s] %s" (show x) (show ty)
  show PBoolType             = "Bool"
  show PIntegerType          = "Int"
  show PFloatType            = "Float"
  show PStringType           = "String"
  show (PTypeArg ty te)      = printf "TypeArg %s %s" (show ty) (show te)
}
