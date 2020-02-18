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
                    ["[","]"]
                    ,["{}"]
                   ,["{\"a\": 0.0 }"]
                   ,["{","}"]
                   ,["[1,\"c\",[]]"]
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
refObjectResults = [[EmptyArray]
                    ,[EmptyObject]
                    ,[OpenDelim Object,TokenText "a",TokenSeparator Colon,TokenNumber 0.0,CloseDelim Object]
                    ,[EmptyObject]
                    ,[OpenDelim Array,TokenNumber 1.0,TokenSeparator Comma,TokenText "c",TokenSeparator Comma,EmptyArray,CloseDelim Array] ]

refObjectArrayDigest :: [[(SnocList Accessor, PrimVal)]]
refObjectArrayDigest  =[[(RNil,PrimEmptyArray)]
                        ,[(RNil,PrimEmptyObject)]
                        ,[((:|) RNil (ObjectField "a"),PrimNumber 0.0)],[(RNil,PrimEmptyObject)]
                        ,[((:|) RNil (ArrayIx 0),PrimNumber 1.0),((:|) RNil (ArrayIx 1),PrimString "c"),((:|) RNil (ArrayIx 2),PrimEmptyArray)]]

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
  case outputStreams of
      Right os ->    if os  /=   refObjectArrayDigest
                       then error "bad outputStreams digest!"
                       else return ()
  return ()
