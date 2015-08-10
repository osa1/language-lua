{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexer/Parsec interface
module Text.Parsec.LTok
  ( tok
  , tok'
  , anyIdent
  , number
  , stringlit
  , Parser
  ) where

import Language.Lua.Annotated.Lexer (LTok, AlexPosn(..))
import Language.Lua.Token

import Text.Parsec hiding (satisfy, string)

type Parser = Parsec [LTok] ()

-- | This parser succeeds whenever the given predicate returns true when called with
-- parsed `LTok`. Same as 'Text.Parsec.Char.satisfy'.
satisfy :: (Stream [LTok] m LTok) => (LTok -> Bool) -> ParsecT [LTok] u m LToken
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: LTok -> Maybe LToken
    tokeq t = if f t then Just (fst t) else Nothing

satisfy' :: (Stream [LTok] m LTok) => (LTok -> Maybe a) -> ParsecT [LTok] u m a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> LTok -> [LTok] -> SourcePos
nextPos pos _ ((_, AlexPn _ l c):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                    = pos

-- | Parses given `LToken`.
tok :: (Stream [LTok] m LTok) => LToken -> ParsecT [LTok] u m LToken
tok t = satisfy (\(t', _) -> t' == t) <?> show t

tok' :: (Stream [LTok] m LTok) => LToken -> ParsecT [LTok] u m ()
tok' p = tok p >> return ()

-- | Parses a `LTokIdent`.
anyIdent :: Monad m => ParsecT [LTok] u m String
anyIdent = satisfy' p <?> "ident"
  where p (t, _) = case t of LTokIdent i -> Just i
                             _ -> Nothing

-- | Parses a `LTokNum`.
number :: Monad m => ParsecT [LTok] u m String
number = satisfy' p <?> "number"
  where p (t, _) = case t of LTokNum n -> Just n
                             _ -> Nothing

-- | Parses a `LTokSLit`.
stringlit :: Monad m => ParsecT [LTok] u m String
stringlit = satisfy' p <?> "string"
  where p (t, _) = case t of LTokSLit s -> Just s
                             _ -> Nothing
