{
module Kite.Parser where
import Data.List
import Kite.Lexer
import Text.Printf

mkBinopCall op a1 a2 = PCall (PCall (PIdentifier op) a1) a2

mkCalls f args = foldl PCall (PCall f (head args)) (tail args)

mkFunc params body =
  let ini = PFunc (PFuncType (head params) (PFreeType "t"))
      fns = foldl (\fn param ->
                    fn . PReturn . PFunc (PFuncType param (PFreeType "t"))
                  ) ini (tail params)
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
        bool               { TBool _ $$ }

        intTy              { TType _ "Int" }
        floatTy            { TType _ "Float" }
        stringTy           { TType _ "String" }
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
        : BinOp            { $1 }
        | List             { $1 }
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

Assign :: { Expr }
        : id '=' Expr      { PAssign (PIdentifier $1) $3 }

-- differentiate between standard and function blocks
StandardBlock :: { Expr }
               : '{' Stmts '}'    { PBlock StandardBlock $2 }

FuncBlock :: { Expr }
           : '{' Stmts '}'    { mkFuncBlock $2 }

List   :: { Expr }
List    : '[' Exprs ']'    { PList $2 }

BinOp  :: { Expr }
        : Expr operator Expr  { mkBinopCall $2 $1 $3 }

Type   :: { Type }
        : boolTy           { PBoolType }
        | intTy            { PIntegerType }
        | floatTy          { PFloatType }
        | stringTy         { PStringType }
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
TypeList :: { [Type] }
          : {- nothing -}      { [] }
          | Type               { [$1] }
          | Type ',' TypeList  { $1 : $3 }

-- primitive types
Term     :: { Expr }
          : int              { PInteger $1 }
          | bool             { PBool $1 }
          | float            { PFloat $1 }
          | string           { PString $1 }
          | id               { PIdentifier $1 }
          | '(' operator ')' { PIdentifier $2 }

{

-- error handling
happyError (x:xs) = error $ "Parse error: " ++ (show . posn2str . tok2posn) x

posn2str (AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col

data BlockType = StandardBlock
               | FuncBlock
               deriving (Show, Eq)

data Expr = PList [Expr]
          | PBlock BlockType [Expr]
          | PIf Expr Expr Expr
          | PAssign Expr Expr -- PIdentifier!
          | PFunc Type Expr -- PFuncType!
          | PCall Expr Expr -- PIdentifier!
          | PReturn Expr

          | PInteger Int
          | PFloat Float
          | PBool Bool
          | PString String
          | PIdentifier String
          deriving (Show, Eq)

data Type = PListType Type
          | PFuncType Type Type
          | PBoolType
          | PIntegerType
          | PFloatType
          | PStringType
          | PTypeArg Type Expr -- PIdentifier!
          | PFreeType String
          | PVoidType
          deriving (Eq)

instance Show Type where
  show (PListType ty)        = printf "[%s]" $ show ty
  show (PFuncType param ret) = printf "(%s -> %s)" (show param) (show ret)
  show PBoolType             = "Bool"
  show PIntegerType          = "Int"
  show PVoidType             = "Void"
  show PFloatType            = "Float"
  show PStringType           = "String"
  show (PFreeType id)        = id
  show (PTypeArg te ty)      = printf "%s: %s" (show ty) (show te)
}
