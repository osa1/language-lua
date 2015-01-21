module Main where

import           Control.Applicative
import           Criterion.Main
import           Data.Maybe          (catMaybes)
import           System.Directory    (getDirectoryContents)
import           System.FilePath

import           Language.Lua

main :: IO ()
main = defaultMain
  [ env (loadFiles "lua-5.2.2-tests") $ \files ->
      bench "Parsing Lua files from 5.2.2 test suite" $
        nf (catMaybes . map (either (const Nothing) Just) . map (parseText chunk)) files
  ]

loadFiles :: FilePath -> IO [String]
loadFiles root = do
  luaFiles <- map (root </>) . filter ((==) ".lua" . takeExtension) <$> getDirectoryContents root
  mapM readFile luaFiles
