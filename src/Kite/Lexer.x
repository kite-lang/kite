{
module Kite.Lexer where
}

%wrapper "posn"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\(\)\{\}\[\]\;\,\:\_]
$operatorSymbols        = [\+\-\/\*\%\=\|\&\<\>\!\~\`\#\.\:\^\$]

@keywords		= return | import | if | then | else | yolo | match
@operators		= [$operatorSymbols]+
@string                 = \" (. # \")* \"
@char                   = \' (\\?. # \') \'
@identifier		= $downcase [$alphaNum \_ \' \! \?]*
@bool			= "True" | "False"
@void                   = "Void"
@typedecl               = "::"
@type			= $upcase [$alphaNum]*
@comment		= "--" .*
@multilineComment	= "{-" ($white | .)* "-}"

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ \p s -> TKeyword p s }
  @operators		{ \p s -> TOperator p s }
  @typedecl             { \p s -> TOperator p s }
  $digit+\.$digit+	{ \p s -> TFloat p (read s) }
  $digit+"f"		{ \p s -> TFloat p (read $ init s) }
  $digit+		{ \p s -> TInteger p (read s) }
  @bool		        { \p s -> TBool p (read s) }
  @void		        { \p s -> TVoid p }
  $symbols		{ \p s -> TSymbol p (head s) }

  @string               { \p s -> TString p $ (tail . init) s }
  @char                 { \p s -> TChar p (read s) }
  @identifier		{ \p s -> TIdentifier p s }
  @type			{ \p s -> TType p s }

{

-- alexEOF :: Alex Token
-- alexEOF = return EOF

data Token = TSymbol     AlexPosn Char
           | TIdentifier AlexPosn String
           | TType       AlexPosn String
           | TInteger    AlexPosn Int
           | TFloat      AlexPosn Float
           | TBool       AlexPosn Bool
           | TVoid       AlexPosn
           | TString     AlexPosn String
           | TChar       AlexPosn Char
           | TKeyword    AlexPosn String
           | TOperator   AlexPosn String
           | TEOF        AlexPosn
           deriving (Eq,Show)

-- get the AlexPosn from a token
tok2posn (TSymbol     p _) = p
tok2posn (TIdentifier p _) = p
tok2posn (TType       p _) = p
tok2posn (TInteger    p _) = p
tok2posn (TFloat      p _) = p
tok2posn (TString     p _) = p
tok2posn (TChar       p _) = p
tok2posn (TBool       p _) = p
tok2posn (TVoid       p  ) = p
tok2posn (TKeyword    p _) = p
tok2posn (TOperator   p _) = p
tok2posn (TEOF        p  ) = p
}
