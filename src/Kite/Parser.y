{
module Kite.Parser where
import Kite.Lexer
}

%name kiteparser
%tokentype { Token }
%error { parseError }

%token
        int                { Integer $$ }
        float              { Float $$ }
        string             { String $$ }
        primType           { Type $$ }
        id                 { Identifier $$ }

        '+'                { BinOp "+" }
        '-'                { BinOp "-" }
        '*'                { BinOp "*" }
        '/'                { BinOp "/" }
        '%'                { BinOp "%" }
        '=='               { BinOp "==" }
        '<'                { BinOp "<" }
        '<='               { BinOp "<=" }
        '>'                { BinOp ">" }
        '>='               { BinOp ">=" }
        '!='               { BinOp "!=" }

        return             { Keyword "return" }
        if                 { Keyword "if" }
        then               { Keyword "then" }
        else               { Keyword "else" }
        yolo               { Keyword "yolo" }

        '='                { Operator "=" }
        '#'                { Operator "#" }
        '->'               { Operator "->" }

        '('                { Symbol '(' }
        ')'                { Symbol ')' }
        '['                { Symbol '[' }
        ']'                { Symbol ']' }
        '{'                { Symbol '{' }
        '}'                { Symbol '}' }
        ','                { Symbol ',' }
        ';'                { Symbol ';' }

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

Assign  : Type id '=' Expr      { PAssign $1 (PIdentifier $2) $4 }

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

Type    : primType         { PPrimType $1 }
        | '[' primType ']' { PListType $2 }
        | FuncType         { $1 }

TypeArg : Type id          { PTypeArg $1 (PIdentifier $2) }

-- support both single expr and blocks
If    : if Expr then Expr else Expr    { PIf $2 (PBlock [$4]) (PBlock [$6]) }
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
        | float            { PFloat $1 }
        | string           { PString $1 }
        | id               { PIdentifier $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr = PBinOp String Expr Expr -- Operator!
          | PTerm PTerm
          | PList [Expr]
          | PBlock [Expr]
          | PIf Expr Expr Expr
          | PAssign Type PTerm Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PGroup Expr -- PFuncType!
          | PCall PTerm [Expr] -- PIdentifier!
          | PReturn Expr
          | PIndex Expr Expr
          deriving (Show, Eq)

data Type = PListType String
          | PFuncType [Type] Type
          | PPrimType String
          | PTypeArg Type PTerm -- PIdentifier!
          deriving (Show, Eq)

data PTerm = PInteger Int
           | PFloat Float
           | PString String
           | PIdentifier String
          deriving (Show, Eq)
}
