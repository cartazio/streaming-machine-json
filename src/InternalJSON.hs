{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module InternalJSON
    (
     Accessor (..)
    , PrimVal (..)
    , SnocList (..)
    , eventor
    , parserState
    , batchFeed
    , batchHarness
    , batchChunkedParse
    , batchHarnessMachina
    , chunkedParse
    , snoc2List
    , list2Snoc
    ,JError(..)
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
-- import Control.Monad.Except
import Control.Monad.Catch
import Control.Exception(Exception,SomeException)

import Data.Data (Data)
import Data.Typeable (Typeable)

import GHC.Generics (Generic)

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString.Char8 as BSC

import Data.Machine


import Control.Monad.ST.Strict (ST)
import InternalParserJSON
import  StreamJSON

-- import Data.Resampler.Types.Error


data JError =
    BadState    [JsonToken]  (SnocList DelimSort)  (SnocList Accessor )
  | LexingError String
  | EarlyTerm   [JsonToken]  (SnocList DelimSort)  (SnocList Accessor )
  | InitOrFinal  JsonToken
  | InvalidSeq  [JsonToken]  (SnocList DelimSort)  (SnocList Accessor)
  deriving(Eq,Ord,Show,Data,Typeable)

instance Exception JError

snoc2List :: SnocList a -> [a]
snoc2List =  reverse . raw2List
  where
    raw2List (as :| a) = a : raw2List as
    raw2List RNil = []


list2Snoc :: [a] -> SnocList a
list2Snoc = snocReverse . list2raw
  where
    list2raw (a : as) = list2raw  as :| a
    list2raw [] = RNil


snocReverse :: SnocList a -> SnocList a
snocReverse = snRev  RNil
  where
    snRev ls RNil = ls
    snRev ls (as :| a) = snRev (ls :| a) as


-- Convert a parser for `a` into a model that accepts `Bytestring` chunks and
-- produces values of type `a`.
{-# SPECIALIZE  chunkedParseGeneral ::  ABC.Parser a -> ProcessT IO BSC.ByteString a#-}
{-# SPECIALIZE  chunkedParseGeneral ::  ABC.Parser a -> ProcessT (Either SomeException) BSC.ByteString a#-}
{-# SPECIALIZE  chunkedParseGeneral ::  ABC.Parser a -> ProcessT (ST s) BSC.ByteString a#-}
chunkedParseGeneral :: forall m a  . MonadThrow  m => ABC.Parser a -> ProcessT m BSC.ByteString a
chunkedParseGeneral parser = construct $ await >>=   startParse parser


startParse :: forall m a b . MonadThrow m =>  ABC.Parser a -> BSC.ByteString -> PlanT (Is BSC.ByteString) a m b
startParse parser = \bs -> start bs

  where
    start :: BSC.ByteString -> PlanT (Is BSC.ByteString) a m b
    start bs = do
      result <- ABC.parseWith (await <|> return BSC.empty) parser bs
      case result of
        ABC.Fail remains contexts message ->  lift $ throwM $ LexingError $ "ChunkedParseGeneral: "++message++" remains: " ++ show remains ++ "contexts: " ++ show contexts
        ABC.Partial _ -> error "chunkedParseGeneral: parseWith produced Partial."
        ABC.Done remains res -> yield res >>
                                if BSC.null remains then  stop else start remains

-- Process `Bytestring` chunks into `JsonToken`s.
{-# SPECIALIZE  chunkedParse :: ProcessT IO BSC.ByteString JsonToken#-}
chunkedParse :: forall   m . MonadThrow  m => ProcessT m BSC.ByteString JsonToken
chunkedParse = chunkedParseGeneral $
               do
                 _<-ABC.many' ABC.space
                 p  <-  parseTokenPrimValue
                 _<- ABC.many' ABC.space
                 return p


token2JPrimVal:: JsonToken ->Maybe PrimVal
token2JPrimVal (TokenText t)   = Just $ PrimString t
token2JPrimVal (TokenBool b)   = Just $ PrimBool b
token2JPrimVal (TokenNumber n) = Just $ PrimNumber n
token2JPrimVal (TokenNull)     = Just PrimNull
token2JPrimVal EmptyArray      = Just PrimEmptyArray
token2JPrimVal EmptyObject     = Just PrimEmptyObject
token2JPrimVal (OpenDelim _ )  = Nothing
token2JPrimVal (CloseDelim _ )  = Nothing
token2JPrimVal (TokenSeparator _ )  = Nothing

-- TODO: should empty arrays and objects be primvalues?
-- TODO: We think that ... "f":,"g": ... will sneak through
{-# SPECIALIZE eventor :: ProcessT IO JsonToken (SnocList Accessor,PrimVal) #-}
{-# SPECIALIZE eventor :: ProcessT (Either SomeException) JsonToken (SnocList Accessor,PrimVal) #-}
{-# SPECIALIZE eventor :: ProcessT (ST s) JsonToken (SnocList Accessor,PrimVal) #-}
-- | 'eventor' acts as a token stream validator, for valid JSON objects. If you
-- cant assume
eventor :: forall m e . MonadThrow   m
        => ProcessT m JsonToken (SnocList Accessor,PrimVal)
eventor = construct $ parserState [] RNil RNil


parserState :: forall m res .
              MonadThrow m =>
              [JsonToken]
            -> SnocList DelimSort
            -> SnocList Accessor
            -> PlanT (Is JsonToken) (SnocList Accessor,PrimVal) m res

parserState (TokenSeparator Comma : rest) e2@(_ :| Open Array) (acslist :| ArrayIx i) =
    parserState rest e2 (acslist :| ArrayIx (i+1))

parserState (OpenDelim Object : TokenText t : TokenSeparator Colon : rest) dlist acslist =
    parserState rest (dlist :| Open Object) (acslist :| ObjectField t)

parserState (OpenDelim Array: rest) dlist acslist =
    parserState rest (dlist :| Open Array) (acslist :| ArrayIx 0)

-- the close delim leads are for nested cases
parserState e1@(CloseDelim Object : rest) e2@(dlist :| dhead) e3@(acslist :| achead) =
    case (dhead, achead) of
      (Open Object, ObjectField _) -> parserState rest dlist acslist
      _                            -> lift $ throwM $ BadState e1 e2 e3

parserState e1@(CloseDelim Array : rest) e2@(dlist :| dhead) e3@(acslist :| achead) =
    case (dhead,achead) of
      (Open Array, ArrayIx _) -> parserState rest dlist acslist
      _                       -> lift $ throwM $ BadState e1 e2 e3

parserState e1@(primv : TokenSeparator Comma : rest) e2@(_ :| Open Array) e3@(acslist :| ArrayIx i)
  | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest e2 (acslist :| ArrayIx (i+1))
  | otherwise                        = lift $ throwM $ BadState e1 e2 e3

parserState e1@(primv : CloseDelim Array : rest)
            e2@(dlist:| Open Array)
            e3@(acslist :| ArrayIx _i)
  | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest dlist acslist
  | otherwise                        = lift $ throwM $ BadState e1 e2 e3

parserState e1@(primv : CloseDelim Object : rest)
            e2@(dlist :| Open Object)
            e3@(acslist :| ObjectField _t)
  | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest dlist acslist
  | otherwise                        = lift $ throwM $ BadState e1 e2 e3
parserState [] RNil RNil = await >>= initOrFinalDelim

parserState e1@(primv : TokenSeparator Comma : TokenText t : TokenSeparator Colon : rest)
            e2@(_dlist :| Open Object)
            e3@(acslist :| ObjectField _f)
  | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest e2 (acslist :| ObjectField t)
  | otherwise                        = lift $ throwM  $ BadState e1 e2 e3

parserState _e1@(TokenSeparator Comma : TokenText t : TokenSeparator Colon : rest)
            e2@(_dlist :| Open Object)
            _e3@(acslist :| ObjectField _f)
  = parserState rest e2 (acslist :| ObjectField t)

parserState context nesting path
  | length context <= 4 = (await <|> (lift . throwM $ EarlyTerm context nesting path)) >>= (\tok -> parserState (context ++ [tok]) nesting path)
  | otherwise = lift $ throwM  $ InvalidSeq context nesting path

initOrFinalDelim :: MonadThrow m => JsonToken -> PlanT (Is JsonToken) (SnocList Accessor, PrimVal) m res
initOrFinalDelim od
 | od == OpenDelim Object || od == OpenDelim Array = parserState [od] RNil RNil
initOrFinalDelim od = lift $ throwM $ InitOrFinal od




-- Testing Functions --
batchFeed :: [a] -> Source a
batchFeed x = construct $ mapM_ yield x >> stop

batchHarness :: MonadThrow  m
             => [BSC.ByteString] -> m [(SnocList Accessor,PrimVal)]
batchHarness x = runT $ batchHarnessMachina x

batchChunkedParse :: MonadThrow  m => [BSC.ByteString] -> MachineT m k JsonToken
batchChunkedParse x = batchFeed x ~> chunkedParse


batchHarnessMachina :: MonadThrow  m =>
     [BSC.ByteString]
     -> MachineT m k (SnocList Accessor, PrimVal)
batchHarnessMachina x = batchFeed x ~> chunkedParse ~>  eventor