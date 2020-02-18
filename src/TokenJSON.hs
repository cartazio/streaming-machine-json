{-# LANGUAGE DeriveFoldable,DeriveTraversable,DeriveGeneric, DeriveDataTypeable,DeriveFunctor #-}

module TokenJSON (JsonToken (..), DelimSort (..), Separator (..), ConType (..)) where

import Data.Text
import Data.Data
import GHC.Generics

data JsonToken = OpenDelim  !ConType | CloseDelim !ConType | TokenSeparator !Separator | TokenText !Text | TokenBool !Bool | TokenNull | TokenNumber !Double | EmptyArray | EmptyObject
  deriving (Eq,Ord,Show,Generic,Data,Typeable)

data DelimSort = Open  ConType | Close ConType
   deriving (Eq,Ord,Show,Generic,Data,Typeable)

data Separator = Colon | Comma
  deriving (Eq,Ord,Show,Generic,Data,Typeable)
data ConType = Object | Array
  deriving (Eq,Ord,Show,Generic,Data,Typeable)
