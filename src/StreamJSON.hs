{-# LANGUAGE DeriveFoldable,DeriveTraversable,DeriveGeneric, DeriveDataTypeable,DeriveFunctor #-}
module StreamJSON (SnocList (..), Accessor (..), PrimVal (..)) where
import Data.Text
import Data.Typeable
import GHC.Generics
import Data.Foldable
import Data.Traversable

data Accessor
  = ArrayIx Int
  | ObjectField Text
  deriving (Eq,Ord,Show,Generic,Typeable)

data PrimVal
  = PrimString !Text
  | PrimNumber !Double
  | PrimBool !Bool
  | PrimNull
  | PrimEmptyArray
  | PrimEmptyObject
  deriving (Eq,Ord,Show,Generic,Typeable)

infixl  5 :|

data SnocList a
  = RNil
  | (:|) (SnocList a) a
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show)
