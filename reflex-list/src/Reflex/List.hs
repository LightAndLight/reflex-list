{-# language RecursiveDo #-}
{-# language StandaloneDeriving #-}
module Reflex.List where

import Reflex
import Reflex.Network

import Control.Lens.Operators ((<&>))
import Control.Monad.Fix (MonadFix)
import Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import Data.Functor.Identity (Identity(..))
import Text.Show (showString, showParen)

data ListF f a
  = Nil
  | Cons (f a) (f (ListF f a))

instance (Eq1 f, Eq a) => Eq (ListF f a) where
  Nil == Nil = True
  Cons a b == Cons c d = eq1 a c && eq1 b d
  _ == _ = False

instance (Show1 f, Show a) => Show (ListF f a) where
  showsPrec d Nil = showString "Nil"
  showsPrec d (Cons a as) =
    showParen (d > 10) $
    showString "Cons " .
    showsPrec1 11 a .
    showString " " .
    showsPrec1 11 as

type List = ListF Identity

mkListF
  :: (Reflex t, MonadHold t m, Adjustable t m, MonadFix m)
  => Event t a
  -> m (Dynamic t (ListF (Dynamic t) a))
mkListF eCons = do
  (eHeadCons, eTailCons) <- headTailE eCons
  networkHold
    (pure Nil)
    (eHeadCons <&> \a -> do
        (eHeadCons', eTailCons') <- headTailE eTailCons
        dHead <- holdDyn a eTailCons
        dTail <-
          networkHold
            (pure Nil)
            (current dHead <@ eHeadCons' <&> go eTailCons dHead)
        pure $ Cons dHead dTail)
  where
    go eCons dHead a = do
      (eHeadCons, eTailCons) <- headTailE eCons
      dHead <- holdDyn a $ current dHead <@ eCons
      dTail <-
        networkHold
          (pure Nil)
          (current dHead <@ eHeadCons <&> go eTailCons dHead)
      pure $ Cons dHead dTail

distributeF :: (Reflex t, Applicative m) => Dynamic t (ListF (Dynamic t) a) -> Dynamic t (ListF m a)
distributeF = (>>= go)
  where
    go :: (Reflex t, Applicative m) => ListF (Dynamic t) a -> Dynamic t (ListF m a)
    go Nil = pure Nil
    go (Cons a as) = Cons <$> (pure <$> a) <*> (pure <$> distributeF as)

staticList :: List a -> [a]
staticList Nil = []
staticList (Cons (Identity a) (Identity as)) = a : staticList as
