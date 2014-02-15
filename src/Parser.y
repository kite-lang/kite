{
module Parser where
import Lexer
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
        binop              { BinOp $$ }
        '='                { Operator "=" }
        '->'               { Operator "->" }
        '('                { Symbol '(' }
        ')'                { Symbol ')' }
        '['                { Symbol '[' }
        ']'                { Symbol ']' }
        '{'                { Symbol '{' }
        '}'                { Symbol '}' }
        ','                { Symbol ',' }
        ';'                { Symbol ';' }

%%

Stmt    : Expr             { $1 }

Stmts   : {- nothing -}    { [] }
        | Stmt             { [$1] }
        | Stmt ';' Stmts   { $1 : $3 }

Expr    : BinOp            { $1 }
        | List             { $1 }
        | Block            { $1 }
        | Assign           { $1 }
        | Func             { $1 }
        | Term             { PTerm $1 }
        | '(' Expr ')'     { PGroup $2 }

Exprs   : {- nothing -}    { [] }
        | Expr             { [$1] }
        | Expr ',' Exprs   { $1 : $3 }

-- Expression rules
Assign  : id '=' Expr      { PAssign (PIndentifier $1) $3 }

Block   : '{' Stmts '}'    { PBlock $2 }

List    : '[' Exprs ']'    { PList $2 }

BinOp   : Expr binop Expr  { PBinOp $2 $1 $3 }

Func    : FuncDef Block    { PFunc $1 $2 }

Type    : primType         { PPrimType $1 }
        | '[' primType ']' { PListType $2 }
        | FuncType         { $1 }

TypeArg : Type id          { PTypeArg $1 (PIndentifier $2) }

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
        | id               { PIndentifier $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr = PBinOp String Expr Expr -- Operator!
          | PTerm PTerm
          | PList [Expr]
          | PBlock [Expr]
          | PAssign PTerm Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PGroup Expr -- PFuncType!
          deriving Show

data Type = PListType String
          | PFuncType [Type] Type
          | PPrimType String
          | PTypeArg Type PTerm -- PIdentifier!
          deriving Show

data PTerm = PInteger Int
           | PFloat Float
           | PString String
           | PIndentifier String
          deriving Show
}
