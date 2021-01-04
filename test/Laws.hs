{-# language FlexibleInstances #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Applicative (liftA2)
import Data.Proxy (Proxy(..))
import GHC.Generics
import Rose
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Classes

main :: IO ()
main = do
  lawsCheckMany laws

laws :: [(String, [Laws])]
laws =
  [ ("Rose"
    , [ applicativeLaws rose1
      -- , genericLaws (Proxy @(Rose a))
      -- , generic1Laws (Proxy @Rose)
      , eqLaws rose
      , foldableLaws rose1
      , functorLaws rose1
      , monadLaws rose1
      , monadZipLaws rose1
      , ordLaws rose
      , showLaws rose
      , showReadLaws rose
      , traversableLaws rose1
      -- , comonadLaws rose1
      ]
    )
  ]

type NonCommutativeMonoid = [Int]

rose :: Proxy (Rose NonCommutativeMonoid)
rose = Proxy

rose1 :: Proxy Rose
rose1 = Proxy

instance Arbitrary a => Arbitrary (Rose a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary1 Rose where
  liftArbitrary gen = sized $ \n -> do
    k <- chooseInt (0, n)
    go k
    where
      go n = do -- n is the size of the trees
        value <- gen
        pars <- genPartition (n - 1) -- can go negative!
        subtrees <- mapM go pars
        pure $ Rose value subtrees

      genPartition :: Int -> Gen [Int]
      genPartition k = case compare k 1 of
        LT -> pure []
        EQ -> pure [1]
        GT -> do
          first <- chooseInt (1, k)
          rest <- genPartition (k - first)
          shuffle (first : rest)

  liftShrink s = go
    where
      go (Rose value subtrees) = subtrees
        ++ [ Rose a as
           | (a, as) <- liftShrink2 s (liftShrink go) (value, subtrees)
           ]
