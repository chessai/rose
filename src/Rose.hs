{-# language
    DeriveDataTypeable
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , ImportQualifiedPost
  , InstanceSigs
  , MultiParamTypeClasses
  , PackageImports
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
  , ViewPatterns
#-}

module Rose
  ( Rose
  , pattern Rose
  ) where

import "base" Control.Monad.Zip (MonadZip)
import "base" Data.Coerce (coerce)
import "base" Data.Functor.Classes (Eq1, Ord1, Show1, Read1)
import "base" GHC.Generics (Generic, Generic1)
import "comonad" Control.Comonad (Comonad(..))
import "free" Control.Comonad.Cofree (Cofree(..))
import "free" Control.Comonad.Cofree qualified as Cofree
--import "indexed-traversable" Data.Foldable.WithIndex (FoldableWithIndex)
--import "indexed-traversable" Data.Functor.WithIndex (FunctorWithIndex)
--import "indexed-traversable" Data.Traversable.WithIndex (TraversableWithIndex(..))

newtype Rose a = MkRose (Cofree [] a)
  deriving stock
    ( Read
    , Show
    )
  deriving stock
    ( Generic
    , Generic1
    )
  deriving newtype
    ( Applicative
    , Eq
    , Eq1
    , Foldable
    --, FoldableWithIndex [Int]
    , Functor
    --, FunctorWithIndex [Int]
    , Monad
    , MonadZip
    , Ord
    , Ord1
    , Read1
    , Show1
    )

pattern Rose :: a -> [Rose a] -> Rose a
pattern Rose a as <- (pat -> (a, as))
  where
    Rose x xs = MkRose (x :< coerce xs)
{-# complete Rose #-}

pat :: Rose a -> (a, [Rose a])
pat (MkRose (a :< as)) = (a, coerce as)
{-# inline pat #-}

instance Traversable Rose where
  traverse :: forall f a b. (Applicative f) => (a -> f b) -> Rose a -> f (Rose b)
  traverse f = fmap (coerce @(Cofree [] b) @(Rose b)) . traverse f . coerce
  {-# inline traverse #-}

instance Comonad Rose where
  extend f w@(MkRose c) = MkRose (f w :< fmap (extend (f . MkRose)) (Cofree.unwrap c))
  duplicate w@(MkRose c) = MkRose (w :< fmap (fmap MkRose . duplicate) (Cofree.unwrap c))
  extract (MkRose (a :< _)) = a
  {-# inline extract #-}

--instance TraversableWithIndex [Int] Rose where
--  itraverse f (MkRose (a :< as)) =
--    fmap coerce ((:<) <$> f [] a <*> itraverse (\i -> itraverse (f . (:) i)) as)
--  {-# inline itraverse #-}
