{
module Kite.Parser where
import Kite.Lexer
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
        | return Expr      { PReturn $2 }

Stmts   : {- nothing -}    { [] }
        | Stmt             { [$1] }
        | Stmt ';' Stmts   { $1 : $3 }

Expr    : BinOp            { $1 }
        | List             { $1 }
        | Block            { $1 }
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

-- Expression rules
Call    : id '(' Exprs ')'    { PCall (PIdentifier $1) $3 }

Index   : Expr '#' Expr     { PIndex $1 $3 }

Assign  : id '=' Expr      { PAssign (PIdentifier $1) $3 }

Block   : '{' Stmts '}'    { PBlock $2 }

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


Func    : FuncDef Block    { PFunc $1 $2 }

Type    : boolTy           { PBoolType }
        | intTy            { PIntegerType }
        | floatTy          { PFloatType }
        | stringTy         { PStringType }
        | '[' Type ']'     { PListType $2 }
        | FuncType         { $1 }

TypeArg : Type id          { PTypeArg $1 (PIdentifier $2) }

-- support both single expr and blocks
If    : if Expr then Expr else Expr    { PIf $2 $4 $6 }
      | if Expr then Block else Block  { PIf $2 $4 $6 }

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

data Expr = PBinOp String Expr Expr -- Operator!
          | PTerm Term
          | PList [Expr]
          | PBlock [Expr]
          | PIf Expr Expr Expr
          | PAssign Term Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PGroup Expr -- PFuncType!
          | PCall Term [Expr] -- PIdentifier!
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
          deriving (Show, Eq)

data Term = PInteger Int
          | PFloat Float
          | PBool Bool
          | PString String
          | PIdentifier String
          deriving (Show, Eq)
}
