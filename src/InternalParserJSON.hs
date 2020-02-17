{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module InternalParserJSON(
  JsonToken(..)
  ,Separator(..)
  ,ConType(..)
  ,parseTokenPrimValue
  ,DelimSort(..)
  ) where

import Data.ByteString.Builder
  (Builder, byteString, toLazyByteString, charUtf8, word8)
import Control.Applicative (empty,(<|>))
import Data.Attoparsec.ByteString.Char8 (char,Parser,string,peekChar',double)
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Char (chr, isDigit)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B

import TokenJSON


#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116


-- | 'parseTokenValue' will parse any "token" value, namely true,false,String,Number, or Null
parseTokenPrimValue :: Parser JsonToken
parseTokenPrimValue =  do
    nextChar <- peekChar'
    case nextChar of
      '[' -> ( string "[]" >> return EmptyArray )  <|> (do _<-  char '[' ; return $! OpenDelim Array)
      ']' -> do  _<- char ']'; return $! CloseDelim Array
      '{' -> (do _<-  string "{}" ; return EmptyObject) <|> (do  _ <-char '{' ; return $! OpenDelim Object)
      '}' -> do _<- char '}' ; return $! CloseDelim Object
      ':' -> do _<-  char ':' ; return $! TokenSeparator Colon
      ',' -> do _ <- char ',' ; return $! TokenSeparator Comma
      '"' -> fmap (TokenText $!) jstring  -- Do we really need $! here?
      't' -> do _ <- string "true" ; return $! TokenBool  True
      'f' -> do _ <- string "false" ; return $! TokenBool False
      'n' -> do _<- string "null" ; return TokenNull
      c  | c == '+' || c == '-' || isDigit c ->   fmap TokenNumber double
                                                    --- REPLACE DOUBLE WITH SCIENTIFIC Parse (and eager decimal for 0.0 corner case)
      _ -> empty


-- | Parse a top-level JSON value.  This must be either an object or
-- an array, per RFC 4627.
--
-- The conversion of a parsed value to a Haskell value is deferred
-- until the Haskell value is needed.  This may improve performance if
-- only a subset of the results of conversions are needed, but at a
-- cost in thunk allocation.


-- | Parse a quoted JSON string.
jstring :: Parser Text
jstring = A.word8 DOUBLE_QUOTE *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
jstring_ = {-# SCC "jstring_" #-} do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == DOUBLE_QUOTE
                                        then Nothing
                                        else Just (c == BACKSLASH)
  _ <- A.word8 DOUBLE_QUOTE
  s1 <- if BACKSLASH `B.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
         else return s

  case decodeUtf8' s1 of
      Right r  -> return r
      Left err -> fail $ show err

{-# INLINE jstring_ #-}

unescape :: Z.Parser ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=BACKSLASH)
    let rest = do
          start <- Z.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          if slash /= BACKSLASH || escape == 255
            then fail "invalid JSON escape sequence"
            else do
            let cont m = go (acc `mappend` byteString h `mappend` m)
                {-# INLINE cont #-}
            if t /= 117 -- 'u'
              then cont (word8 (B.unsafeIndex mapping escape))
              else do
                   a <- hexQuad
                   if a < 0xd800 || a > 0xdfff
                     then cont (charUtf8 (chr a))
                     else do
                       b <- Z.string "\\u" *> hexQuad
                       if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                         then let !c = ((a - 0xd800) `shiftL` 10) +
                                       (b - 0xdc00) + 0x10000
                              in cont (charUtf8 (chr c))
                         else fail "invalid UTF-16 surrogates"
    done <- Z.atEnd
    if done
      then return (acc `mappend` byteString h)
      else rest
  mapping = "\"\\/\n\t\b\r\f"

hexQuad :: Z.Parser Int
hexQuad = do
  s <- Z.take 4
  let hex n | w >= C_0 && w <= C_9 = w - C_0
            | w >= C_a && w <= C_f = w - 87
            | w >= C_A && w <= C_F = w - 55
            | otherwise          = 255
        where w = fromIntegral $ B.unsafeIndex s n
      a = hex 0; b = hex 1; c = hex 2; d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "invalid hex escape"


toByteString :: Builder -> ByteString
toByteString = L.toStrict . toLazyByteString
{-# INLINE toByteString #-}