
module Language.Lua.Parser
  ( parseText
  , parseFile
  , stat
  , exp
  , chunk
  ) where

import qualified Language.Lua.Annotated.Parser as A
import Language.Lua.Annotated.Lexer (LTok)
import Language.Lua.Annotated.Simplify
import Language.Lua.Syntax
import Language.Lua.Token

import Text.Parsec
import Text.Parsec.LTok
import Control.Monad (liftM)
import Control.Applicative ((<*), (<$>), (<*>))

parseText :: Parser a -> String -> Either ParseError a
parseText = A.parseText

parseFile :: FilePath -> IO (Either ParseError Block)
parseFile = (liftM . liftM $ sBlock) . A.parseFile

stat :: Parser Stat
stat = fmap sStat A.stat

chunk :: Parser Block
chunk = fmap sBlock A.chunk
