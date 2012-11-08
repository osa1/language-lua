module Langauge.Lua.Test where

--import Data.List (sort)
--import System.Directory (getDirectoryContents)
--import System.FilePath (takeExtension, (</>), splitExtension)
--import Control.Monad (forM)
--import Test.HUnit

import qualified Language.Lua.Parser as Parser
import qualified Language.Lua.PrettyPrinter as Printer
import qualified Language.Lua.Lexer as Lexer

--main :: IO ()
--main = do
--    runLexerTests

--runLexerTests :: IO ()
--runLexerTests = do
--    files <- fmap (map ("tests" </>) . sort . filter luaFile) $ getDirectoryContents "tests"
--    tests <- forM files $ \file -> do
--        contents <- readFile file
--        let tokens = alexScanTokens contents
--        r <- readFile (fst $ splitExtension file)
--        return (file, TestCase $ assertEqual "lexer test" r (show tokens ++ "\n"))
--    let testList = TestList $ map (\(fileName, test) -> TestLabel fileName test) tests
--    putStrLn $ show testList
--    runTestTT testList
--    return ()

--luaFile :: FilePath -> Bool
--luaFile = (== ".lua") . takeExtension
