module TokenJSON (JsonToken (..), DelimSort (..), Separator (..), ConType (..)) where

import Data.Text

data JsonToken = OpenDelim  !ConType | CloseDelim !ConType | TokenSeparator !Separator | TokenText !Text | TokenBool !Bool | TokenNull | TokenNumber !Double | EmptyArray | EmptyObject
  deriving (Eq,Ord,Show)

data DelimSort = Open  ConType | Close ConType
   deriving (Eq,Ord,Show)

data Separator = Colon | Comma
  deriving (Eq,Ord,Show)
data ConType = Object | Array
  deriving (Eq,Ord,Show)
