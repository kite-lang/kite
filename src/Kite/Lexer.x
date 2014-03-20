{
module Kite.Lexer where
}

%wrapper "posn"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\(\)\{\}\[\]\;\,\:]

@keywords		= return | import | if | then | else | yolo
@binops	                = "+" | "-" | "/" | "*" | "%" | "==" | "<" | "<=" | ">" | ">=" | "!="
@operators		= "=" | "#" | "->"
@string                 = \" (. # \")* \"
@identifier		= $downcase [$alphaNum \_ \' \! \?]*
@bool			= "True" | "False"
@type			= $upcase [$alphaNum]*
@comment		= "--" .*
@multilineComment	= "{-" ($white | .)* "-}"

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ tok (\p s -> TKeyword p s) }
  @operators		{ tok (\p s -> TOperator p s) }
  @binops		{ tok (\p s -> TBinOp p s) }

  $digit+\.$digit+	{ tok (\p s -> TFloat p (read s)) }
  $digit+		{ tok (\p s -> TInteger p (read s)) }
  @bool		        { tok (\p s -> TBool p (read s)) }
  $symbols		{ tok (\p s -> TSymbol p (head s)) }

  @string               { tok (\p s -> TString p $ (tail . init) s) }
  @identifier		{ tok (\p s -> TIdentifier p s) }
  @type			{ tok (\p s -> TType p s) }

{

tok f p s = f p s

-- alexEOF :: Alex Token
-- alexEOF = return EOF

data Token
  = TSymbol     AlexPosn Char
  | TIdentifier AlexPosn String
  | TType       AlexPosn String
  | TInteger    AlexPosn Int
  | TFloat      AlexPosn Float
  | TBool       AlexPosn Bool
  | TString     AlexPosn String
  | TKeyword    AlexPosn String
  | TOperator   AlexPosn String
  | TBinOp      AlexPosn String
  | TEOF        AlexPosn
  deriving (Eq,Show)

-- get the AlexPosn from a token
tok2posn (TSymbol     p _) = p
tok2posn (TIdentifier p _) = p
tok2posn (TType       p _) = p
tok2posn (TInteger    p _) = p
tok2posn (TFloat      p _) = p
tok2posn (TString     p _) = p
tok2posn (TBool       p _) = p
tok2posn (TKeyword    p _) = p
tok2posn (TOperator   p _) = p
tok2posn (TBinOp      p _) = p
tok2posn (TEOF        p  ) = p

}
