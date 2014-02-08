{
module Lexer where
}

%wrapper "basic"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\=\+\-\*\/\(\)\>\{\}\;]

@keywords		= return | import | if | then | else | yolo
@operators		= "->"|"="
@identifier		= $downcase [$alphaNum \_ \']*
@type			= $upcase [$alphaNum \_ \']*
@comment		=  \#.*
@multilineComment	= \#\-($white | .)*\-\#

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ \s -> Keyword s }
  @operators		{ \s -> Operator s }

  $digit+\.$digit+	{ \s -> Float (read s) }
  $digit+		{ \s -> Integer (read s) }
  $symbols		{ \s -> Symbol (head s) }
  
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
           deriving (Eq,Show)
} 
