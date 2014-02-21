{
module Kite.Lexer where
}

%wrapper "posn"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\(\)\{\}\[\]\;\,]

@keywords		= return | import | if | then | else | yolo
@binops	                = "+" | "-" | "/" | "*" | "%" | "==" | "<" | "<=" | ">" | ">=" | "!="
@operators		= "=" | "#" | "->"
@string                 = \" ($white | .)* \"
@identifier		= $downcase [$alphaNum \_ \' \! \?]*
@type			= $upcase [$alphaNum]*
@comment		= "--" .*
@multilineComment	= "{-" ($white | .)* "-}"

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ tok (\p s -> Keyword p s) }
  @operators		{ tok (\p s -> Operator p s) }
  @binops		{ tok (\p s -> BinOp p s) }

  $digit+\.$digit+	{ tok (\p s -> Float p (read s)) }
  $digit+		{ tok (\p s -> Integer p (read s)) }
  $symbols		{ tok (\p s -> Symbol p (head s)) }

  @string               { tok (\p s -> String p $ (tail . init) s) }
  @identifier		{ tok (\p s -> Identifier p s) }
  @type			{ tok (\p s -> Type p s) }

{

tok f p s = f p s

-- alexEOF :: Alex Token
-- alexEOF = return EOF

data Token = Symbol     AlexPosn Char
           | Identifier AlexPosn String
           | Type       AlexPosn String
           | Integer    AlexPosn Int
           | Float      AlexPosn Float
           | String     AlexPosn String
           | Keyword    AlexPosn String
           | Operator   AlexPosn String
           | BinOp      AlexPosn String
           | EOF        AlexPosn
           deriving (Eq,Show)

-- get the AlexPosn from a token
tok2posn (Symbol     p _) = p
tok2posn (Identifier p _) = p
tok2posn (Type       p _) = p
tok2posn (Integer    p _) = p
tok2posn (Float      p _) = p
tok2posn (String     p _) = p
tok2posn (Keyword    p _) = p
tok2posn (Operator   p _) = p
tok2posn (BinOp      p _) = p
tok2posn (EOF        p  ) = p
}
