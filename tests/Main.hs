
module Main where

import           Language.Lua.Annotated
import           Language.Lua.Annotated.Lexer

import           Text.Parsec

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad                (forM_)
import           Prelude                      hiding (exp)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stringTests, numberTests]

parseExps :: String -> String -> Either ParseError [Exp SourcePos]
parseExps file contents = runParser (many exp) () file (llex contents)

stringTests :: TestTree
stringTests = testGroup "String tests"
    [ testCase
        "Strings from 5.2.2 reference manual"
        (do let file = "tests/strings"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of strings parsed" (length exps == 5)
                assertEqTrans $ map (fmap (const ())) exps)
    ]
  where
    assertEqTrans :: [Exp ()] -> Assertion
    assertEqTrans [] = return ()
    assertEqTrans [_] = return ()
    assertEqTrans (a : b : rest) = do
      assertEqual "Strings are not same" a b
      assertEqTrans (b : rest)

numberTests :: TestTree
numberTests = testGroup "Number tests"
    [ testCase
        "Numbers from 5.2.2 reference manual"
        (do let file = "tests/numbers"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of numbers parsed" (length exps == 9)
                forM_ exps assertNumber)
    ]
  where
    assertNumber :: Show a => Exp a -> Assertion
    assertNumber Number{} = return ()
    assertNumber nan      = assertFailure ("Not a number: " ++ show nan)

