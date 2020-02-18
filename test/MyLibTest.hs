{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import InternalJSON
import Data.Machine.Type (runT)
import qualified Data.ByteString.Char8 as BSC
import TokenJSON

main :: IO ()
main = do

  output <- mapM (\ str -> runT ({-batchHarnessMachina-} batchChunkedParse str))
                   [["[]"],["{}"]
                   ,["{\"a\": 0.0 }"]
                   ,["[0.0]"]
                   ,[ "0.0"]
                   ,["0.","0"]
                   ,["0",".0"]
                   ]
  mapM print output

  if output /= [[EmptyArray]
                ,[EmptyObject]
                ,[OpenDelim Object,TokenText "a",TokenSeparator Colon,TokenNumber 0.0,CloseDelim Object]
                ,[OpenDelim Array,TokenNumber 0.0,CloseDelim Array]
                ,[TokenNumber 0.0]
                ,[TokenNumber 0.0]
                ,[TokenNumber 0.0]]
    then error "bad program"
    else return ()
