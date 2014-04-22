{
module Kite.Parser where
import Data.List
import Kite.Lexer
import Kite.Syntax
import Text.Printf

--- Desugaring

mkInfixCall op a1 a2 = PCall (PCall (PIdentifier op) a1) a2
-- TODO: "partial infix right" can probably be done cleaner
mkPartialRightInfixCall op rhs = PFunc (PFuncType (PTypeArg (PFreeType "t") (PIdentifier "lhs"))
                                                  (PFreeType "t"))
                                       (PReturn (PCall (PCall (PIdentifier op) (PIdentifier "lhs")) rhs))
mkPartialLeftInfixCall op lhs = PCall (PIdentifier op) lhs

mkCharList str = PList (map PChar str)

--TODO: clean parameterless functions up
mkCalls f args =
  if null args
     then PCall f PVoid
     else foldl PCall (PCall f (head args)) (tail args)

mkFunc params body =
  let firstParam = if null params then PTypeArg PVoidType (PIdentifier "_") else head params
      restParams = if null params then [] else tail params
      ini = PFunc (PFuncType firstParam (PFreeType "t"))
      fns = foldl (\fn param ->
                    fn . PReturn . PFunc (PFuncType param (PFreeType "t"))
                  ) ini restParams
  in fns body

mkFuncBlock exprs =
  case last exprs of
    PReturn _ -> PBlock FuncBlock exprs
    _ -> PBlock FuncBlock (init exprs ++ [PReturn (last exprs)])

}

%name kiteparser
%tokentype { Token }

%token
        int                { TInteger _ $$ }
        float              { TFloat _ $$ }
        string             { TString _ $$ }
        char               { TChar _ $$ }
        bool               { TBool _ $$ }

        intTy              { TType _ "Int" }
        floatTy            { TType _ "Float" }
        charTy             { TType _ "Char" }
        boolTy             { TType _ "Bool" }
        id                 { TIdentifier _ $$ }

        '='                { TOperator _ "=" }
        '->'               { TOperator _ "->" }
        '|'                { TOperator _ "|" }
        '`'                { TOperator _ "`" }

        operator           { TOperator _ $$ }

        import             { TKeyword _ "import" }
        return             { TKeyword _ "return" }
        if                 { TKeyword _ "if" }
        then               { TKeyword _ "then" }
        else               { TKeyword _ "else" }
        yolo               { TKeyword _ "yolo" }

        '('                { TSymbol _ '(' }
        ')'                { TSymbol _ ')' }
        '['                { TSymbol _ '[' }
        ']'                { TSymbol _ ']' }
        '{'                { TSymbol _ '{' }
        '}'                { TSymbol _ '}' }
        ','                { TSymbol _ ',' }
        ':'                { TSymbol _ ':' }
        ';'                { TSymbol _ ';' }

%right in
%nonassoc '&&' '||'
%nonassoc '==' '<' '<=' '>' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

%%

Program :: { Expr }
         : Stmts           { PBlock StandardBlock $1 }

Stmt   :: { Expr }
        : Expr             { $1 }
        | StandardBlock    { $1 }
        | return Expr      { PReturn $2 }

Stmts  :: { [Expr] }
        : {- nothing -}    { [] }
        | Stmt             { [$1] }
        | Stmt Stmts       { $1 : $2 }
        | Stmt ';' Stmts   { $1 : $3 }

Expr   :: { Expr }
        : List             { $1 }
        | Assign           { $1 }
        | Func             { $1 }
        | Call             { $1 }
        | If               { $1 }
        | Term             { $1 }
        | '(' Expr ')'     { $2 }

Exprs  :: { [Expr] }
Exprs   : {- nothing -}    { [] }
        | Expr             { [$1] }
        | Expr ',' Exprs   { $1 : $3 }

-- expression rules
Call   :: { Expr }
        : Expr '(' Exprs ')'    { mkCalls $1 $3 }
        | Expr '`' Expr Expr    { PCall (PCall $3 $1) $4 }
        | Expr operator Expr    { mkInfixCall $2 $1 $3 } -- infix
        | '(' Expr operator ')'    { mkPartialLeftInfixCall $3 $2 } -- partial left infix
        | '(' operator Expr ')'    { mkPartialRightInfixCall $2 $3 } -- partial right infix

Assign :: { Expr }
        : id '=' Expr                  { PAssign (PIdentifier $1) $3 }
        | '{' operator '}' '=' Expr    { PAssign (PIdentifier $2) $5 }

-- differentiate between standard and function blocks
StandardBlock :: { Expr }
               : '{' Stmts '}'    { PBlock StandardBlock $2 }

FuncBlock :: { Expr }
           : '{' Stmts '}'    { mkFuncBlock $2 }

List   :: { Expr }
List    : '[' Exprs ']'    { PList $2 }

Type   :: { Type }
        : boolTy           { PBoolType }
        | intTy            { PIntegerType }
        | floatTy          { PFloatType }
        | charTy           { PCharType }
        | '[' Type ']'     { PListType $2 }
--        | FuncType         { $1 }
        | id               { PFreeType $1 }

-- support both single expr and blocks
If     :: { Expr }
        : if Expr then Expr else Expr    { PIf $2 $4 $6 }
        | if Expr then StandardBlock else StandardBlock  { PIf $2 $4 $6 }

-- functions
Func   :: { Expr }
        : FuncSignature FuncBlock        { mkFunc (fst $1) $2 }
        | '->' FuncBlock                 { mkFunc [] $2 }

-- func literal
FuncSignature :: { ([Type], Type) }
         : '|' Parameters '|' '->'       { ($2, PFreeType "t") }
         | '|' Parameters '|' '->' Type  { ($2, $5) }

-- named arguments
Parameters :: { [Type] }
           : {- nothing -}             { [] }
           | Parameter                 { [$1] }
           | Parameter ',' Parameters  { $1 : $3 }

-- func literal parameter
Parameter :: { Type }
         : id              { PTypeArg (PFreeType "t") (PIdentifier $1) }
         | id ':' Type     { PTypeArg $3 (PIdentifier $1) }

-- func signature
-- FuncType  :: { Type }
--            : '|' TypeList '|' '->' Type { mkFuncType $2 $5 }

-- just type
-- TypeList :: { [Type] }
--           : {- nothing -}      { [] }
--           | Type               { [$1] }
--           | Type ',' TypeList  { $1 : $3 }

-- primitive types
Term     :: { Expr }
          : int              { PInteger $1 }
          | bool             { PBool $1 }
          | float            { PFloat $1 }
          | string           { mkCharList $1 }
          | char             { PChar $1 }
          | id               { PIdentifier $1 }
          | '(' operator ')' { PIdentifier $2 }

{
-- error handling
happyError (x:xs) = error $ "Parse error: " ++ (show . posn2str . tok2posn) x
happyError _ = error $ "Parse error: unexpected end of file"

posn2str (AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col
}
