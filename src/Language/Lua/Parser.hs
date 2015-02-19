module Language.Lua.Parser
  ( parseText
  , parseNamedText
  , parseFile
  , stat
  , exp
  , chunk
  ) where

import           Control.Monad                   (liftM)
import           Prelude                         hiding (exp)
import           Text.Parsec
import           Text.Parsec.LTok

import qualified Language.Lua.Annotated.Parser   as A
import           Language.Lua.Annotated.Simplify
import           Language.Lua.Syntax

parseText :: Parser a -> String -> Either ParseError a
parseText = A.parseText

parseNamedText :: Parser a -> String -> String -> Either ParseError a
parseNamedText = A.parseNamedText

parseFile :: FilePath -> IO (Either ParseError Block)
parseFile = (liftM . liftM $ sBlock) . A.parseFile

stat :: Parser Stat
stat = fmap sStat A.stat

exp :: Parser Exp
exp = fmap sExp A.exp

chunk :: Parser Block
chunk = fmap sBlock A.chunk
