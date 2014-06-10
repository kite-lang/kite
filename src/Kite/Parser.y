{
module Kite.Parser where

import Data.List
import Kite.Lexer
import Kite.Syntax
import Kite.Sugar
import Text.Printf
}

%name kiteparser
%tokentype { Token }

%token
        int                { TInteger _ $$ }
        float              { TFloat _ $$ }
        string             { TString _ $$ }
        char               { TChar _ $$ }
        bool               { TBool _ $$ }
        void               { TVoid _ }

        intTy              { TType _ "Int" }
        floatTy            { TType _ "Float" }
        charTy             { TType _ "Char" }
        boolTy             { TType _ "Bool" }
        id                 { TIdentifier _ $$ }

        '='                { TOperator _ "=" }
        '->'               { TOperator _ "->" }
        '<-'               { TOperator _ "<-" }
        '|'                { TOperator _ "|" }
        '`'                { TOperator _ "`" }
        '::'               { TOperator _ "::" }

        operator           { TOperator _ $$ }

        import             { TKeyword _ "import" }
        return             { TKeyword _ "return" }
        if                 { TKeyword _ "if" }
        then               { TKeyword _ "then" }
        else               { TKeyword _ "else" }
        yolo               { TKeyword _ "yolo" }
        match              { TKeyword _ "match" }

        '('                { TSymbol _ '(' }
        ')'                { TSymbol _ ')' }
        '['                { TSymbol _ '[' }
        ']'                { TSymbol _ ']' }
        '{'                { TSymbol _ '{' }
        '}'                { TSymbol _ '}' }
        ','                { TSymbol _ ',' }
        ';'                { TSymbol _ ';' }
        '_'                { TSymbol _ '_' }

%right in
%nonassoc '&&' '||'
%nonassoc '==' '<' '<=' '>' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

%%

--- Top level

Program :: { [Decl] }
         : Decls { $1 }

Decl   :: { Decl }
        : id '=' Expr                     { PDecl $1 $3 }
        | '{' operator '}' '=' Expr       { PDecl $2 $5 }
        | id '::' Type                { PTypeDecl $1 $3 }
        | '{' operator '}' '::' Type  { PTypeDecl $2 $5 }

Decls  :: { [Decl] }
        : {- nothing -}    { [] }
        | Decl             { [$1] }
        | Decl Decls       { $1 : $2 }
        | Decl ';' Decls   { $1 : $3 }


--- Pattern matching

Match :: { Expr }
       : match Expr '{' Patterns '}'           { PMatch $2 $4 }

Pattern :: { PatternCase }
         : id ',' id             '->' Expr       { (PatCons $1 $3, $5) }
         | '(' id ',' id ')'     '->' Expr       { (PatPair     $2 $4, $7) }
         | Expr                  '->' Expr       { (PatPrimitive $1,   $3) }
         | '_'                   '->' Expr       { (PatOtherwise,      $3) }

Patterns  :: { [PatternCase] }
Patterns   : {- nothing -}      { [] }
        | Pattern               { [$1] }
        | Pattern ',' Patterns  { $1 : $3 }


--- Expressions

Draw   :: { Draw }
        : id '<-' Expr     { PDraw $1 $3 }

Draws  :: { [Draw] }
        : Draw             { [$1]}
        | Draw ',' Draws   { $1 : $3}


Expr   :: { Expr }
        : List             { $1 }
        | Pair             { $1 }
        | Match            { $1 }
        | Bind             { $1 }
        | Lambda           { $1 }
        | Apply            { $1 }
        | ListComp         { $1 }
        | If               { $1 }
        | Term             { $1 }
        | '(' Expr ')'     { $2 }
        | Return           { $1 }

Exprs :: { [Expr] }
        : {- nothing -}    { [] }
        | Expr             { [$1] }
        | Expr ',' Exprs   { $1 : $3 }

Return :: { Expr }
        : return Expr      { PReturn $2 }

Apply   :: { Expr }
        : Expr '(' Exprs ')'    { mkCalls $1 $3 }
        -- infix function application
        | Expr '`' Expr Expr    { PApply (PApply $3 $1) $4 }
        -- infix operator
        | Expr operator Expr    { mkInfixCall $2 $1 $3 }
        -- partial left infix
        | '(' Expr operator ')'    { mkPartialLeftInfixCall $3 $2 }
        -- partial right infix
        | '(' operator Expr ')'    { mkPartialRightInfixCall $2 $3 }
ListComp :: { Expr }
        : '[' Expr '|' Draws '|' Exprs ']' { mkComprehension (mkCompreExpr $2 $4) $4 (mkCompreGuards $6 $4) }
        | '[' Expr '|' Draws ']' { mkComprehension (mkCompreExpr $2 $4) $4 (mkCompreGuards [] $4) }

Bind :: { Expr }
        : id '=' Expr                  { PBind $1 $3 }
        | '{' operator '}' '=' Expr    { PBind $2 $5 }

Block :: { Expr }
           : '{' BlockExprs '}'   { mkBlock $2 }

-- TODO: WHAT THE FUCK!?
BlockExpr :: { Expr }
          : Expr { $1 }

BlockExprs  :: { [Expr] }
BlockExprs   : {- nothing -}         { [] }
             | BlockExpr             { [$1] }
             | BlockExpr BlockExprs  { $1 : $2 }
             | BlockExpr ';' BlockExprs  { $1 : $3 }

List   :: { Expr }
List    : '[' Exprs ']'           { PList $2 }

Pair   :: { Expr }
Pair    : '(' Expr ',' Expr ')'   { PPair $2 $4 }

Type   :: { Type }
        : boolTy             { PBoolType }
        | intTy              { PIntegerType }
        | floatTy            { PFloatType }
        | charTy             { PCharType }
        | '(' Type ',' Type ')' { PPairType $2 $4 }
        | '[' Type ']'       { PListType $2 }
        | id                 { PTypeVar $1 }
        | Type '->' Type     { PLambdaType $1 $3 }

-- TODO: support both single expr and blocks
If     :: { Expr }
        : if Expr then Expr else Expr    { PIf $2 $4 $6 }


--- Lambdas

Lambda   :: { Expr }
        : LambdaSignature Block      { mkFunc $1 $2 }
        | '->' Block                 { mkFunc [] $2 }

LambdaSignature :: { [String] }
         : '|' void '|' '->'             { [] }
         | '|' Parameters '|' '->'       { $2 }

Parameters :: { [String] }
           : {- nothing -}      { [] }
           | id                 { [$1] }
           | id ',' Parameters  { $1 : $3 }


--- Primitives

Term     :: { Expr }
          : int              { PInteger $1 }
          | bool             { PBool $1 }
          | float            { PFloat $1 }
          | string           { mkCharList $1 }
          | char             { PChar $1 }
          | id               { PIdentifier $1 }
          | void             { PVoid }
          | '(' operator ')' { PIdentifier $2 }

{
-- error handling
happyError (x:xs) = error $ "Parse error: " ++ (show . posn2str . tok2posn) x
happyError _ = error $ "Parse error: unexpected end of file"

posn2str (AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col
}
