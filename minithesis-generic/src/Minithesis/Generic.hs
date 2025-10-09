{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Minithesis.Generic
  ( HasStrategy (..),
    genericStrategy,
  )
where

import GHC.Generics
import Minithesis (Strategy, just, mixOf, named, nothing)

-- | Types that can supply a 'Strategy'.
class HasStrategy a where
  strategy :: Strategy a
  default strategy :: (Generic a, GHasStrategy (Rep a)) => Strategy a
  strategy = genericStrategy

-- | Helper to derive strategies from 'Generic' representations.
genericStrategy :: forall a. (Generic a, GHasStrategy (Rep a)) => Strategy a
genericStrategy = fmap to gStrategy

-- | Internal class used to build strategies for generic representations.
class GHasStrategy f where
  gStrategy :: Strategy (f x)

instance GHasStrategy V1 where
  gStrategy = named "empty" (const "empty") nothing

instance GHasStrategy U1 where
  gStrategy = just U1

instance (GHasStrategy f, GHasStrategy g) => GHasStrategy (f :*: g) where
  gStrategy =
    let left :: Strategy (f x)
        left = gStrategy
        right :: Strategy (g x)
        right = gStrategy
     in do
          l <- left
          r <- right
          pure (l :*: r)

instance (GHasStrategy f, GHasStrategy g) => GHasStrategy (f :+: g) where
  gStrategy =
    let left :: Strategy (f x)
        left = gStrategy
        right :: Strategy (g x)
        right = gStrategy
     in mixOf [fmap L1 left, fmap R1 right]

instance (GHasStrategy f) => GHasStrategy (M1 D meta f) where
  gStrategy = fmap M1 gStrategy

instance (Constructor meta, GHasStrategy f) => GHasStrategy (M1 C meta f) where
  gStrategy =
    let constructorName =
          conName (undefined :: M1 C meta f ())
     in named constructorName (const constructorName) (fmap M1 gStrategy)

instance (GHasStrategy f) => GHasStrategy (M1 S meta f) where
  gStrategy = fmap M1 gStrategy

instance (HasStrategy a) => GHasStrategy (K1 i a) where
  gStrategy = fmap K1 (strategy :: Strategy a)

instance HasStrategy Bool where
  strategy = named "bool" show $ mixOf [just False, just True]

instance (HasStrategy a) => HasStrategy [a]

instance (HasStrategy a) => HasStrategy (Maybe a)

instance (HasStrategy a, HasStrategy b) => HasStrategy (Either a b)

instance (HasStrategy a, HasStrategy b) => HasStrategy (a, b)
