{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
module Godot.Internal.Dispatch where
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import GHC.TypeLits as T
import Godot.Gdnative.Internal.Gdnative

-- | Establishes 'child` as a child of BaseClass child`
class HasBaseClass child where
  type BaseClass child
  super :: child -> BaseClass child

-- | Transitive subclass relation. You shouldn't need to define instances of this.
class parent :< child where
  safeCast :: child -> parent

instance {-# OVERLAPPING #-} refl :< refl where
  safeCast = id

instance {-# OVERLAPPABLE #-} (HasBaseClass c, pp :< BaseClass c) => pp :< c where
  safeCast = safeCast .  super

-- |A class method 'name', overriden in 'cls', with signature 'sig'
-- |Attempts to emulate C++/Godot's "virtual" single dispatch
class Method (name :: Symbol) cls sig | cls name -> sig where
  runMethod :: cls -> sig

  

instance {-# OVERLAPPABLE #-} (Method name (BaseClass child) sig, HasBaseClass child) 
    => Method name child sig where
  runMethod = runMethod @name . super

-- this ensures termination for type errors
-- fixes #4
instance {-# OVERLAPPABLE #-} ( TypeError (T.Text "Couldn't find method " :<>: ShowType name
                                          :<>: T.Text " with signature `" :<>: ShowType sig
                                          :<>: T.Text "'.")
                              , Method name GodotObject sig ) -- for fundeps
    => Method name GodotObject sig where
  runMethod = error "unreachable"

newtype Signal a = Signal Text
  deriving (Show, Eq)
