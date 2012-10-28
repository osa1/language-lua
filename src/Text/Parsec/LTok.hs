{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexer/Parsec interface
module Text.Parsec.LTok where

import Language.Lua.Lexer (LTok, AlexPosn(..))
import Language.Lua.Token

import Text.Parsec hiding (satisfy)

type Parser = Parsec [LTok] ()

satisfy :: (Stream [LTok] m LTok) => (LTok -> Bool) -> ParsecT [LTok] u m LTok
satisfy f = tokenPrim show nextPos tokeq
  where nextPos :: SourcePos -> LTok -> [LTok] -> SourcePos
        nextPos pos _ ((_, (AlexPn _ l c)):_) = setSourceColumn (setSourceLine pos l) c
        nextPos pos _ []                  = pos -- TODO: ??

        tokeq :: LTok -> Maybe LTok
        tokeq t = if f t then Just t else Nothing

tok :: (Stream [LTok] m LTok) => LToken -> ParsecT [LTok] u m LTok
tok t = satisfy (\(t', _) -> t' == t) <?> show t

anyIdent :: Monad m => ParsecT [LTok] u m LTok
anyIdent = satisfy p <?> "ident"
  where p (t, _) = case t of LTokIdent _ -> True
                             _ -> False