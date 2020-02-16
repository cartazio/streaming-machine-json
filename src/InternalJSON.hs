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
import Control.Monad.Except

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString.Char8 as BSC

import Data.Machine


import InternalParserJSON
import  StreamJSON

-- import Data.Resampler.Types.Error


data JError =
  BadState [JsonToken]  (SnocList DelimSort)  (SnocList Accessor )
  |LexingError String
  |EarlyTerm  [JsonToken] (SnocList DelimSort) (SnocList Accessor )
  | InitOrFinal  JsonToken
  | InvalidSeq [JsonToken] ( SnocList DelimSort) (SnocList Accessor)
  deriving(Eq,Ord,Show)


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
chunkedParseGeneral :: forall m a  . MonadError JError m => ABC.Parser a -> ProcessT m BSC.ByteString a
chunkedParseGeneral parser = construct $ await >>= start
  where
    start bs = do
      result <- ABC.parseWith (await <|> return BSC.empty) parser bs
      case result of
        ABC.Fail remains contexts message -> throwError . LexingError $ "ChunkedParseGeneral: "++message++" remains: " ++ show remains ++ "contexts: " ++ show contexts
        ABC.Partial _ -> error "chunkedParseGeneral: parseWith produced Partial."
        ABC.Done remains res -> yield res >>
                                if BSC.null remains then stop else start remains

-- Process `Bytestring` chunks into `JsonToken`s.
chunkedParse :: forall   m . MonadError JError m => ProcessT m BSC.ByteString JsonToken
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
eventor :: forall m e . MonadError JError  m
        => ProcessT m JsonToken (SnocList Accessor,PrimVal)
eventor = construct $ parserState [] RNil RNil
  where
    parserState :: [JsonToken]
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
          _                            -> throwError $ BadState e1 e2 e3

    parserState e1@(CloseDelim Array : rest) e2@(dlist :| dhead) e3@(acslist :| achead) =
        case (dhead,achead) of
          (Open Array, ArrayIx _) -> parserState rest dlist acslist
          _                       -> throwError $ BadState e1 e2 e3

    parserState e1@(primv : TokenSeparator Comma : rest) e2@(_ :| Open Array) e3@(acslist :| ArrayIx i)
      | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest e2 (acslist :| ArrayIx (i+1))
      | otherwise                        = throwError $ BadState e1 e2 e3

    parserState e1@(primv : CloseDelim Array : rest)
                e2@(dlist:| Open Array)
                e3@(acslist :| ArrayIx _i)
      | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest dlist acslist
      | otherwise                        = throwError $ BadState e1 e2 e3

    parserState e1@(primv : CloseDelim Object : rest)
                e2@(dlist :| Open Object)
                e3@(acslist :| ObjectField _t)
      | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest dlist acslist
      | otherwise                        = throwError $ BadState e1 e2 e3
    parserState [] RNil RNil = await >>= initOrFinalDelim

    parserState e1@(primv : TokenSeparator Comma : TokenText t : TokenSeparator Colon : rest)
                e2@(_dlist :| Open Object)
                e3@(acslist :| ObjectField _f)
      | (Just v) <- token2JPrimVal primv = yield (e3,v) >> parserState rest e2 (acslist :| ObjectField t)
      | otherwise                        = throwError  $ BadState e1 e2 e3

    parserState _e1@(TokenSeparator Comma : TokenText t : TokenSeparator Colon : rest)
                e2@(_dlist :| Open Object)
                _e3@(acslist :| ObjectField _f)
      = parserState rest e2 (acslist :| ObjectField t)

    parserState context nesting path
      | length context <= 4 = (await <|> (lift . throwError $ EarlyTerm context nesting path)) >>= (\tok -> parserState (context ++ [tok]) nesting path)
      | otherwise = throwError  $ InvalidSeq context nesting path

    initOrFinalDelim :: JsonToken -> PlanT (Is JsonToken) (SnocList Accessor, PrimVal) m res
    initOrFinalDelim od
     | od == OpenDelim Object || od == OpenDelim Array = parserState [od] RNil RNil
    initOrFinalDelim od = throwError $ InitOrFinal od



-- Testing Functions --
batchFeed :: [a] -> Source a
batchFeed x = construct $ mapM_ yield x >> stop

batchHarness :: MonadError JError m
             => [BSC.ByteString] -> m [(SnocList Accessor,PrimVal)]
batchHarness x = runT $ batchHarnessMachina x

batchChunkedParse :: MonadError JError m => [BSC.ByteString] -> MachineT m k JsonToken
batchChunkedParse x = batchFeed x ~> chunkedParse


batchHarnessMachina :: MonadError JError m =>
     [BSC.ByteString]
     -> MachineT m k (SnocList Accessor, PrimVal)
batchHarnessMachina x = batchFeed x ~> chunkedParse ~>  eventor