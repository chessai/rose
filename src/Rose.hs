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

import "base" GHC.Read qualified as Read
import "base" Text.ParserCombinators.ReadPrec qualified as Read
import "base" Text.Read.Lex qualified as Read
import "base" Control.Monad.Zip (MonadZip)
import "base" Data.Coerce (coerce)
import "base" Data.Functor.Classes (Eq1, Ord1, Show1(..), Read1(..), showsBinaryWith)
import "base" GHC.Show (showSpace)
import "base" GHC.Generics (Generic, Generic1)
import "comonad" Control.Comonad (Comonad(..))
import "free" Control.Comonad.Cofree (Cofree(..), ComonadCofree(..))
import "free" Control.Comonad.Cofree qualified as Cofree
--import "indexed-traversable" Data.Foldable.WithIndex (FoldableWithIndex)
--import "indexed-traversable" Data.Functor.WithIndex (FunctorWithIndex)
--import "indexed-traversable" Data.Traversable.WithIndex (TraversableWithIndex(..))

newtype Rose a = MkRose (Cofree [] a)
  deriving stock
    ( Generic
    , Generic1
    )
  deriving newtype
    ( Applicative
    , ComonadCofree []
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
    )

instance (Show a) => Show (Rose a) where
  showsPrec d (Rose a as) = id
    $ showParen (d >= 11)
    $ showString "Rose "
    . showsPrec 11 a
    . showSpace
    . showsPrec 11 as

instance Show1 Rose where
  liftShowsPrec :: forall a. ()
    => (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> Rose a
    -> ShowS
  liftShowsPrec sp sl = go
    where
      goList = liftShowList sp sl
      go p (Rose a as) = id
        $ showParen (p > 11)
        $ showString "Rose "
        . sp 11 a
        . showSpace
        . liftShowsPrec go goList 11 as

instance Read a => Read (Rose a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.expectP (Read.Ident "Rose")
    a <- Read.step Read.readPrec
    as <- Read.step Read.readPrec
    pure (Rose a as)

{-instance Read1 Rose where
  liftReadsPrec rp rl = go
    where
      goList = liftReadList rp rl
      go d r = flip (readParen (d > 10)) r $ \r' -> [ ]-}
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

pattern Rose :: a -> [Rose a] -> Rose a
pattern Rose a as <- (pat -> (a, as))
  where
    Rose x xs = MkRose (x :< coerce xs)
{-# complete Rose #-}

pat :: Rose a -> (a, [Rose a])
pat (MkRose (a :< as)) = (a, coerce as)
{-# inline pat #-}

coiter :: (a -> [a]) -> a -> Rose a
coiter = coerce Cofree.coiter

coiterW :: (Comonad w) => (w a -> [w a]) -> w a -> Rose a
coiterW f w = MkRose (Cofree.coiterW f w)

unfold :: (b -> (a, [b])) -> b -> Rose a
unfold un seed = MkRose (Cofree.unfold un seed)

unfoldM :: (Monad m) => (b -> m (a, [b])) -> b -> m (Rose a)
unfoldM un seed = fmap MkRose (Cofree.unfoldM un seed)

_extract :: (Functor f) => (a -> f a) -> Rose a -> f (Rose a)
_extract f (MkRose a) = fmap MkRose (Cofree._extract f a)

_unwrap :: (Functor f)
  => ([Rose a] -> f [Rose a])
  -> Rose a
  -> f (Rose a)
_unwrap f (Rose a as) = (Rose a) <$> f as

telescoped :: (Functor f)
  => [(Rose a -> f (Rose a)) -> [Rose a] -> f [Rose a]]
  -> (a -> f a)
  -> Rose a
  -> f (Rose a)
telescoped = foldr (\l r -> _unwrap . l . r) _extract

telescoped_ :: (Functor f)
  => [(Rose a -> f (Rose a)) -> [Rose a] -> f [Rose a]]
  -> (Rose a -> f (Rose a))
  -> Rose a
  -> f (Rose a)
telescoped_ = foldr (\l r -> _unwrap . l . r) id

shoots :: (Applicative f)
  => (a -> f a)
  -> Rose a
  -> f (Rose a)
shoots f = go
  where
    go r@(Rose a as)
      | null as = pure r
      | otherwise = Rose <$> f a <*> traverse go as

leaves :: (Applicative f)
  => (a -> f a)
  -> Rose a
  -> f (Rose a)
leaves f = go
  where
    go (Rose a as)
      | null as = (flip Rose as) <$> f a
      | otherwise = Rose a <$> traverse go as
