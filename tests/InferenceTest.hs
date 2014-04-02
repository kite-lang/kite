module InferenceTest (inferenceTests) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Error

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

fn    = PFuncType
free  = PFreeType
bool  = PBoolType
int   = PIntegerType
str   = PStringType
ls    = PListType
float = PFloatType

data TypeCheckError = TypeE | RefE | ArE | UnE deriving(Show, Eq)

nullEnvironment :: Environment
nullEnvironment = Environment { sym = [Map.empty],
                                symCount = 0 }

parse prog = do
  env <- case (typeCheck False . kiteparser . alexScanTokens) prog of
    Right env' -> return env'
    Left err -> error (show err) >> return nullEnvironment
  Map.assocs $ head (sym env)

-- test env
test name prog ex  = testCase name $ case Map.lookup (fst ex) (Map.fromList (parse prog)) of
  Just t -> t @?= snd ex
  Nothing -> error ("Expected identifier not found: " ++ fst ex)

-- test env with basic types and/or simple functions
base = "yes = True; one = 1; half = 0.5; str = \"foo\";"
extra = base ++ "id = |x| -> { return x };"

testEnv name prog ex = test name (base ++ prog) ex
testExt name prog ex = testEnv name (extra ++ prog) ex

inferenceTests = testGroup "Inference test"
  [ testGroup "Primitive"
    [ test "Bool assignment"
      "yes = True"
      ("yes", bool)

    , test "Int assignment"
      "one = 1"
      ("one", int)

    , test "Float assignment"
      "flt = 1.0"
      ("flt", float)

    , test "String assignment"
      "str = \"foo\""
      ("str", str)

    , test "List assignment"
      "ls = [1, 2, 3]"
      ("ls", ls int)
    ]

  , testGroup "List"
    [ test "Primitive"
      "ls = [1, 2]"
      ("ls", ls int)

    , test "Infer type of index expr"
      "head = |xs| -> { return xs # 0 }"
      --TODO: fix these tx vars, it's impossible to track
      ("head", fn [ls (free "t4")] (free "t4"))
    ]

  , testGroup "Function"
    [ test "Simple function"
      "id = |e| -> { return e }"
      ("id", fn [free "t2"] (free "t2"))

    , testExt "Apply polymorhpic function to value"
      "foo = id(2)"
      ("foo", int)
    ]

  , testGroup "Higher order functions"
    [ test "Free function and free param"
      "apply = |f, x| -> { return f(x) }"
      ("apply", fn [fn [free "t3"] (free "t5"), free "t3"] (free "t5"))

    , test "Free function and free param"
      "apply = |f, x| -> { return f(x) }\
      \fn =  |x| -> { return [x] }\
      \val = apply(fn, 1)"
      ("apply", fn [fn [free "t3"] (free "t5"), free "t3"] (free "t5"))


    , testExt "Application of returned function (HoF)"
      "id = |x| -> { return x}\
      \one = id(|x| -> { return x})(1)"
      ("one", int)

    , testExt "Multiple nested applications of returned functions (HoF)"
      "id = |x| -> { return x }\
      \one = id(|x| -> { \
      \    return |y| -> {\
      \       return x + y\
      \    }\
      \})(1)(1)"
     ("one", int)
    ]
  ]
