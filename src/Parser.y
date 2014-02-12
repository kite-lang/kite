{
module Parser where
import Lexer
}

%name kiteparser
%tokentype { Token }
%error { parseError }

%token
        int              { Integer $$ }
        '+'              { Symbol '+' }
%%

Expr    : Expr '+' Expr  { BinOp '+' $1 $3 }
        | Term           { Term $1 }

Term    : int            { PInteger $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr = BinOp Char Expr Expr
          | Term Term
          deriving Show

data Term = PInteger Int
          deriving Show
}
