{-# LANGUAGE DeriveFoldable,DeriveTraversable,DeriveGeneric, DeriveDataTypeable,DeriveFunctor, CPP #-}
module StreamJSON (SnocList (..), Accessor (..), PrimVal (..)) where
import Data.Text
import Data.Typeable
#if MIN_VERSION_base(4,6 ,0)
import GHC.Generics
#endif
import Data.Foldable
import Data.Traversable

data Accessor
  = ArrayIx Int
  | ObjectField Text
  deriving (Eq,Ord,Show,Typeable,Generic)

data PrimVal
  = PrimString !Text
  | PrimNumber !Double
  | PrimBool !Bool
  | PrimNull
  | PrimEmptyArray
  | PrimEmptyObject
  deriving (Eq,Ord,Show,Typeable,Generic)

infixl  5 :|

data SnocList a
  = RNil
  | (:|) (SnocList a) a
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show,Generic)
