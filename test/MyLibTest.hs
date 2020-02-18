{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main (main) where


import InternalJSON
import Data.Machine.Type (runT)
import qualified Data.ByteString.Char8 as BSC
import TokenJSON
import Control.Exception (SomeException)



referenceLiterals :: [[BSC.ByteString]]
referenceLiterals = referenceNotObjects ++ referenceObjects


referenceObjects :: [[BSC.ByteString]]
referenceObjects= [

                    ["{}"]
                   ,["{\"a\": 0.0 }"]
                   ,["{","}"]
                ]

referenceNotObjects   :: [[BSC.ByteString]]
referenceNotObjects = [ ["[]"]
                    ,["[0.0]"]
                   ,[ "0.0"]
                   ,["0.","0"]
                   ,["0",".0"]
                   ,["[{\"a\" : 21}, \"hello\\u29E71\\t\nworld\"]"]
                   ,["true","false","null"]
                   ]


refNotObjectResults :: [[JsonToken]]
refNotObjectResults = [[EmptyArray],[OpenDelim Array,TokenNumber 0.0,CloseDelim Array]
                    ,[TokenNumber 0.0],[TokenNumber 0.0],[TokenNumber 0.0]
                    ,[OpenDelim Array,OpenDelim Object,TokenText "a",TokenSeparator Colon,TokenNumber 21.0
                              ,CloseDelim Object,TokenSeparator Comma,TokenText "hello\10727\&1\t\nworld",CloseDelim Array]
                    ,[TokenBool True,TokenBool False,TokenNull]]



refObjectResults :: [[JsonToken]]
refObjectResults = [[EmptyObject]
                    ,[OpenDelim Object,TokenText "a",TokenSeparator Colon,TokenNumber 0.0,CloseDelim Object]
                    ,[EmptyObject] ]



main :: IO ()
main = do

  outputTokens <- mapM (\ str -> runT ({-batchHarnessMachina-} batchChunkedParse str))
                   referenceLiterals
  print outputTokens

  if outputTokens /= ( refNotObjectResults ++
                      refObjectResults
                    )
    then  error $ "bad program\n "  ++ show outputTokens ++ "\n"
    else return ()
  putStrLn "and now we look at the digestion"
  outputStreams:: Either SomeException [[(SnocList Accessor, PrimVal)]] <- return $  mapM  (\ str -> runT (batchHarnessMachina str))
                      referenceObjects
  putStrLn "now we look at those summaries"
  print outputStreams
  return ()
