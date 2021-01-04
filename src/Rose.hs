{-# language
    DeriveDataTypeable
  , DeriveFunctor
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , PackageImports
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
  , ViewPatterns
#-}

{-# options_ghc -Wall #-}

-- | <https://en.wikipedia.org/wiki/Rose_tree Rose Trees> are trees with
--   an unbounded number of branches per node. Each node contains a value
--   and zero or more subtrees.
module Rose
  ( Rose
  , pattern Rose
  , singleton
  , coiter
  , coiterW
  , unfold
  , unfoldM
  , telescoped
  , telescoped_
  , shoots
  , leaves
  ) where

import "base" Control.Monad.Zip (MonadZip)
import "base" Data.Coerce (coerce)
import "base" Data.Functor.Classes (Eq1, Ord1, Show1(..), Read1(..), readBinaryWith)
import qualified "base" Data.List as List
import "base" GHC.Generics (Generic, Generic1)
import qualified "base" GHC.Read as Read
import "base" GHC.Show (showSpace)
import "base" Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified "base" Text.ParserCombinators.ReadPrec as Read
import qualified "base" Text.Read.Lex as Read
import "comonad" Control.Comonad (Comonad(..))
import "free" Control.Comonad.Cofree (Cofree(..), ComonadCofree(..))
import qualified "free" Control.Comonad.Cofree as Cofree

-- | A Rose tree. This type can be produced and consumed using the
--   @Rose@ pattern.
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
    , Functor
    , Monad
    , MonadZip
    , Ord
    , Ord1
    )

instance forall a. (Show a) => Show (Rose a) where
  showsPrec :: Int -> Rose a -> ShowS
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

instance forall a. Read a => Read (Rose a) where
  readPrec :: ReadPrec (Rose a)
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.expectP (Read.Ident "Rose")
    a <- Read.step Read.readPrec
    as <- Read.step Read.readPrec
    pure (Rose a as)

instance Read1 Rose where
  liftReadPrec :: forall a. ()
    => ReadPrec a
    -> ReadPrec [a]
    -> ReadPrec (Rose a)
  liftReadPrec rp rl = Read.parens
    $ Read.prec 10
    $ readBinaryWith rp (liftReadPrec goShoots goLeaves) "Rose" Rose
    where
      goShoots = do
        ra <- rp
        pure $ singleton ra
      goLeaves = do
        ras <- rl
        pure $ List.map singleton ras

instance Traversable Rose where
  traverse :: forall f a b. (Applicative f) => (a -> f b) -> Rose a -> f (Rose b)
  traverse f = fmap (coerce @(Cofree [] b) @(Rose b)) . traverse f . coerce
  {-# inline traverse #-}

instance Comonad Rose where
  extend f w@(MkRose c) = MkRose (f w :< fmap (extend (f . MkRose)) (Cofree.unwrap c))
  duplicate w@(MkRose c) = MkRose (w :< fmap (fmap MkRose . duplicate) (Cofree.unwrap c))
  extract (MkRose (a :< _)) = a
  {-# inline extract #-}

pattern Rose :: a -> [Rose a] -> Rose a
pattern Rose a as <- (pat -> (a, as))
  where
    Rose x xs = MkRose (x :< coerce xs)
{-# complete Rose #-}

pat :: Rose a -> (a, [Rose a])
pat (MkRose (a :< as)) = (a, coerce as)
{-# inline pat #-}

-- | Generate a singleton rose tree.
--   It has no leaves and one shoot.
--
-- >>> singleton @Int 3
-- Rose 3 []
singleton :: a -> Rose a
singleton a = Rose a []
{-# inline singleton #-}

-- | Use coiteration to generate a
--   rose tree from a seed.
--
--   The coiteration terminates when
--   the generating function returns
--   an empty list:
--
-- >>> 'coiter' (\i -> if i > 3 then [] else [i + 1]) 0
-- Rose 0 [Rose 1 [Rose 2 [Rose 3 [Rose 4 []]]]]
--
--   An infinite, lazy generator for
--   the fibonacci sequence:
--
-- >>> take 10 $ map fst $ 'Data.Foldable.toList' $ 'coiter' (\(a, b) -> [(b, a + b)]) (0, 1)
coiter :: (a -> [a]) -> a -> Rose a
coiter = coerce Cofree.coiter

-- | Like 'coiter' for comonadic values.
--
coiterW :: (Comonad w) => (w a -> [w a]) -> w a -> Rose a
coiterW f w = MkRose (Cofree.coiterW f w)

-- | Unfold a rose tree from a seed.
unfold :: (b -> (a, [b])) -> b -> Rose a
unfold un seed = MkRose (Cofree.unfold un seed)

-- | Unfold a rose tree from a seed, monadically.
unfoldM :: (Monad m) => (b -> m (a, [b])) -> b -> m (Rose a)
unfoldM un seed = fmap MkRose (Cofree.unfoldM un seed)

-- | This is a lens that can be used to read or write from the target of 'extract'.
--
--   Using @^.@ from the @lens@ package:
--
-- @
-- foo ^. '_extract' == 'extract' foo
-- @
--
-- For more lenses see the @lens@ package
--
-- @
-- '_extract' :: Lens' ('Rose' a) a
-- @
_extract :: (Functor f) => (a -> f a) -> Rose a -> f (Rose a)
_extract f (MkRose a) = fmap MkRose (Cofree._extract f a)

-- | This is a lens that can be used to read or write to the tails of a rose tree.
--
--   Using @^.@ from the @lens@ package:
--
-- @
-- foo ^. '_unwrap' == 'unwrap' foo
-- @
--
-- For more lenses see the @lens@ package
--
-- @
-- '_unwrap' :: Lens' ('Rose' a) ['Rose' a]
-- @
_unwrap :: (Functor f)
  => ([Rose a] -> f [Rose a])
  -> Rose a
  -> f (Rose a)
_unwrap f (Rose a as) = (Rose a) <$> f as

-- | Construct an @Lens@ into a rose tree given a list of lenses into the base functor.
--
--   When the input list is empty, this is equivalent to '_extract'.
--   When the input list is non-empty, this composes the input lenses
--   with '_unwrap' to walk through the rose tree before using
--   '_extract' to get the element at the final location.
--
-- For more on lenses see the @lens@ package on hackage.
--
-- @telescoped :: [Lens' ['Rose' a] ('Rose' a)]      -> Lens' ('Rose' a) a@
--
-- @telescoped :: [Traversal' ['Rose' a] ('Rose' a)] -> Traversal' ('Rose' a) a@
--
-- @telescoped :: [Getter ['Rose' a] ('Rose' a)]     -> Getter ('Rose' a) a@
--
-- @telescoped :: [Fold ['Rose' a] ('Rose' a)]       -> Fold ('Rose' a) a@
--
-- @telescoped :: [Setter' ['Rose' a] ('Rose' a)]    -> Setter' ('Rose' a) a@
telescoped :: (Functor f)
  => [(Rose a -> f (Rose a)) -> [Rose a] -> f [Rose a]]
  -> (a -> f a)
  -> Rose a
  -> f (Rose a)
telescoped = foldr (\l r -> _unwrap . l . r) _extract

-- | Construct an @Lens@ into a rose tree given a list of lenses into the base functor.
--
--   The only difference between this and 'telescoped' is that 'telescoped' focuses on a single value, but this focuses on the entire remaining subtree.
--   When the input list is empty, this is equivalent to 'id'.
--   When the input list is non-empty, this composes the input lenses
--   with '_unwrap' to walk through the rose tree.
--
-- For more on lenses see the @lens@ package on hackage.
--
-- @telescoped :: [Lens' ['Rose' a] ('Rose' a)]      -> Lens' ('Rose' a) ('Rose' a)@
--
-- @telescoped :: [Traversal' ['Rose' a] ('Rose' a)] -> Traversal' ('Rose' a) ('Rose' a)@
--
-- @telescoped :: [Getter ['Rose' a] ('Rose' a)]     -> Getter ('Rose' a) ('Rose' a)@
--
-- @telescoped :: [Fold ['Rose' a] ('Rose' a)]       -> Fold ('Rose' a) ('Rose' a)@
--
-- @telescoped :: [Setter' ['Rose' a] ('Rose' a)]    -> Setter' ('Rose' a) ('Rose' a)@
telescoped_ :: (Functor f)
  => [(Rose a -> f (Rose a)) -> [Rose a] -> f [Rose a]]
  -> (Rose a -> f (Rose a))
  -> Rose a
  -> f (Rose a)
telescoped_ = foldr (\l r -> _unwrap . l . r) id

-- | A @Traversal'@ that gives access to all non-leaf elements of a rose tree,
--   where non-leaf is defined as @x@ from @Rose x xs@ where @null xs@ is @False@.
--
--   Because this doesn't give access to all values in the rose tree, it cannot be used to change types (use 'traverse' for that).
shoots :: (Applicative f)
  => (a -> f a)
  -> Rose a
  -> f (Rose a)
shoots f = go
  where
    go r@(Rose a as)
      | null as = pure r
      | otherwise = Rose <$> f a <*> traverse go as

-- | A @Traversal'@ that gives access to all leaf elements of a rose tree, where
--   leaf is defined as @x@ from @Rose x xs@ where @null xs@ is @True@.
--
--   Because this doesn't give access to all values in the rose tree, it cannot
--   be used to change types (use 'traverse' for that).
leaves :: (Applicative f)
  => (a -> f a)
  -> Rose a
  -> f (Rose a)
leaves f = go
  where
    go (Rose a as)
      | null as = (flip Rose as) <$> f a
      | otherwise = Rose a <$> traverse go as
