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
        '+'                { Operator "+" }
        '['                { Symbol '[' }
        ']'                { Symbol ']' }
        ','                { Symbol ',' }

%%

Expr    : BinOp            { $1 }
        | List             { $1 }
        | Term             { Term $1 }

Exprs   : {- nothing -}    { [] }
        | Expr             { [$1] }
        | Expr ',' Exprs   { $1 : $3 }

List    : '[' Exprs ']'    { PList $2 }

BinOp   : Expr '+' Expr    { BinOp '+' $1 $3 }

Term    : int              { PInteger $1 }
        | float            { PFloat $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr = BinOp Char Expr Expr
          | Term Term
          | PList [Expr]
          deriving Show

data Term = PInteger Int
          | PFloat Float
          deriving Show
}
