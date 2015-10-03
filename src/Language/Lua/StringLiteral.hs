{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Language.Lua.StringLiteral
  ( interpretStringLiteral
  , constructStringLiteral
  ) where

import           Data.Char (ord, chr, isNumber, isPrint, isAscii)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List (foldl')
import           Data.Bits ((.&.),shiftR)
import           Numeric (showHex)

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mempty, mappend, mconcat)
#endif

skipWS :: String -> String
skipWS (' '  : rest) = skipWS rest
skipWS ('\n' : rest) = skipWS rest
skipWS ('\r' : rest) = skipWS rest
skipWS ('\f' : rest) = skipWS rest
skipWS ('\t' : rest) = skipWS rest
skipWS ('\v' : rest) = skipWS rest
skipWS str           = str

hexToInt :: Char -> Int
hexToInt c =
  case c of
    'A' -> 10
    'a' -> 10
    'B' -> 11
    'b' -> 11
    'C' -> 12
    'c' -> 12
    'D' -> 13
    'd' -> 13
    'E' -> 14
    'e' -> 14
    'F' -> 15
    'f' -> 15
    _   -> decToNum c

{-# INLINE decToNum #-}
decToNum :: Char -> Int
decToNum c = fromEnum c - fromEnum '0'


interpretStringLiteral :: String -> Maybe ByteString
interpretStringLiteral xxs =
  case xxs of
    '\'':xs -> Just (decodeEscapes (dropLast 1 xs))
    '"':xs  -> Just (decodeEscapes (dropLast 1 xs))
    '[':xs  -> removeLongQuotes xs
    _       -> Nothing

-- | Long-quoted string literals have no escapes.
-- A leading newline on a long quoted string literal is ignored.
removeLongQuotes :: String -> Maybe ByteString
removeLongQuotes str =
  case span (=='=') str of
    (eqs,'[':'\n':xs) -> go (dropLast (2+length eqs) xs)
    (eqs,'[':     xs) -> go (dropLast (2+length eqs) xs)
    _                 -> Nothing
  where
  go = Just . B.toLazyByteString . mconcat . map encodeChar

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

decodeEscapes :: String -> ByteString
decodeEscapes = B.toLazyByteString . aux
  where
  aux xxs =
    case xxs of
      [] -> mempty
      '\\' : 'x' : h1 : h2 : rest ->
        B.word8 (fromIntegral (hexToInt h1 * 16 + hexToInt h2)) `mappend` aux rest

      '\\' : 'u' : '{' : rest ->
        case break (=='}') rest of
          (ds,_:rest')
             | code <= 0x10ffff -> encodeChar (chr code) `mappend` aux rest'
             where code = foldl' (\acc d -> acc * 16 + hexToInt d) 0 ds
          _ -> encodeChar '\xFFFD' `mappend` aux (dropWhile (/='}') rest)

      '\\' : c1 : c2 : c3 : rest
        | isNumber c1 && isNumber c2 && isNumber c3 ->
            let code = decToNum c1 * 100 + decToNum c2 * 10 + decToNum c3
            in B.word8 (fromIntegral code) `mappend` aux rest

      '\\' : c1 : c2 : rest
        | isNumber c1 && isNumber c2 ->
            let code = decToNum c1 * 10 + decToNum c2
            in B.word8 (fromIntegral code) `mappend` aux rest

      '\\' : c1 : rest
        | isNumber c1 -> B.word8 (fromIntegral (decToNum c1)) `mappend` aux rest

      '\\' : 'a'  : rest -> B.char8 '\a' `mappend` aux rest
      '\\' : 'b'  : rest -> B.char8 '\b' `mappend` aux rest
      '\\' : 'f'  : rest -> B.char8 '\f' `mappend` aux rest
      '\\' : 'n'  : rest -> B.char8 '\n' `mappend` aux rest
      '\\' : '\n' : rest -> B.char8 '\n' `mappend` aux rest
      '\\' : 'r'  : rest -> B.char8 '\r' `mappend` aux rest
      '\\' : 't'  : rest -> B.char8 '\t' `mappend` aux rest
      '\\' : 'v'  : rest -> B.char8 '\v' `mappend` aux rest
      '\\' : '\\' : rest -> B.char8 '\\' `mappend` aux rest
      '\\' : '"'  : rest -> B.char8 '"' `mappend` aux rest
      '\\' : '\'' : rest -> B.char8 '\'' `mappend` aux rest
      '\\' : 'z'  : rest -> aux (skipWS rest)
      c : rest -> encodeChar c `mappend` aux rest

-- | Convert a string literal body to string literal syntax
constructStringLiteral :: ByteString -> String
constructStringLiteral bs = '"' : aux 0
  where
  aux i
    | i >= B.length bs = "\""
    | otherwise =
    case B8.index bs i of
      '\a' -> '\\' : 'a'  : aux (i+1)
      '\b' -> '\\' : 'b'  : aux (i+1)
      '\f' -> '\\' : 'f'  : aux (i+1)
      '\n' -> '\\' : 'n'  : aux (i+1)
      '\r' -> '\\' : 'r'  : aux (i+1)
      '\t' -> '\\' : 't'  : aux (i+1)
      '\v' -> '\\' : 'v'  : aux (i+1)
      '\\' -> '\\' : '\\' : aux (i+1)
      '\"' -> '\\' : '"'  : aux (i+1)
      x | isPrint x && isAscii x -> x : aux (i+1)
        | x <= '\x0f' -> '\\' : 'x' : '0' : showHex (ord x) (aux (i+1))
        | otherwise   -> '\\' : 'x'       : showHex (ord x) (aux (i+1))

encodeChar :: Char -> B.Builder
encodeChar c
   | oc <= 0x7f       = asByte oc

   | oc <= 0x7ff      = asByte (0xc0 + (oc `shiftR` 6))
              `mappend` asByte (0x80 + oc .&. 0x3f)

   | oc <= 0xffff     = asByte (0xe0 + (oc `shiftR` 12))
              `mappend` asByte (0x80 + ((oc `shiftR` 6) .&. 0x3f))
              `mappend` asByte (0x80 + oc .&. 0x3f)

   | otherwise        = asByte (0xf0 + (oc `shiftR` 18))
              `mappend` asByte (0x80 + ((oc `shiftR` 12) .&. 0x3f))
              `mappend` asByte (0x80 + ((oc `shiftR` 6) .&. 0x3f))
              `mappend` asByte (0x80 + oc .&. 0x3f)
  where
    asByte = B.word8 . fromIntegral
    oc = ord c
