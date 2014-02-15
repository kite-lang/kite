{
module Lexer where
}

%wrapper "basic"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\(\)\{\}\[\]\;\,]

@keywords		= return | import | if | then | else | yolo
@binops		    = "+" | "-" | "/" | "*"
@operators		= "=" | "->"
@string                 = \" ($white | .)* \"
@identifier		= $downcase [$alphaNum \_ \' \! \?]*
@type			= $upcase [$alphaNum]*
@comment		= "--" .*
@multilineComment	= "{-" ($white | .)* "-}"

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ \s -> Keyword s }
  @operators		{ \s -> Operator s }
  @binops		{ \s -> BinOp s }

  $digit+\.$digit+	{ \s -> Float (read s) }
  $digit+		{ \s -> Integer (read s) }
  $symbols		{ \s -> Symbol (head s) }

  @string               { \s -> String $ (tail . init) s }
  @identifier		{ \s -> Identifier s }
  @type			{ \s -> Type s }


{
data Token = Symbol Char
           | Identifier String
           | Type String
           | Integer Int
           | Float Float
           | String String
           | Keyword String
           | Operator String
           | BinOp String
           deriving (Eq,Show)
}
