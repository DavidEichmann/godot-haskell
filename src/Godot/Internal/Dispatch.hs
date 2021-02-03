{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Godot.Internal.Dispatch(HasBaseClass(..),(:<)(..),Signal(..)) where
import qualified Data.Text
import GHC.TypeLits as T
import Godot.Gdnative.Internal.Gdnative hiding (Signal)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Control.Monad
import Data.List
import Data.Kind as K
import Type.Errors

-- | Establishes 'child` as a child of BaseClass child`
class HasBaseClass child where
  type BaseClass child
  super :: child -> BaseClass child

-- | Subclass relation.
class parent :< child  where
  upcast :: child -> parent

instance (CheckInheritance parent child child, parent :<< child) => parent :< child where
  upcast = upcast'

-- Check inheritance and output a custom type error.
type family CheckInheritance parent child childOrig :: Constraint where
  CheckInheritance a a _ = ()
  CheckInheritance parent child childOrig = IfStuck
    (BaseClass child)
    (DelayError (ErrorInheritance parent childOrig))
    (Pure (CheckInheritance parent (BaseClass child) childOrig))
type ErrorInheritance parent child =
  Text "No instance for " :<>: ShowType parent :<>: Text " :< " :<>: ShowType child

-- | Subclass relation without custom type errors.
class parent :<< child where
  upcast' :: child -> parent

instance {-# OVERLAPPING #-} a :<< a where
  upcast' = id

instance  {-# OVERLAPPABLE #-}  (HasBaseClass child, parent :<< BaseClass child) => parent :<< child where
  upcast' = upcast' . super

newtype Signal a = Signal Data.Text.Text
  deriving (Show, Eq)
