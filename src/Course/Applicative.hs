{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative where

import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Applicative` type-class must satisfy four laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. pure id <*> x = x`
--
-- * The law of composition
--   `∀u v w. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
--
-- * The law of homomorphism
--   `∀f x. pure f <*> pure x = pure (f x)`
--
-- * The law of interchange
--   `∀u y. u <*> pure y = pure ($ y) <*> u`

class Functor f => Applicative f where
  pure ::
    a -> f a
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

  {-
  Known as "spaceship" or "apply"
  -}

infixl 4 <*>

-- | Insert into ExactlyOne.
--
-- prop> \x -> pure x == ExactlyOne x
--
-- >>> ExactlyOne (+10) <*> ExactlyOne 8
-- ExactlyOne 18
instance Applicative ExactlyOne where
  pure ::
    a
    -> ExactlyOne a
  pure =
    error "todo: Course.Applicative pure#instance ExactlyOne"
  (<*>) ::
    ExactlyOne (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<*>) =
    error "todo: Course.Applicative (<*>)#instance ExactlyOne"

-- | Insert into a List.
--
-- prop> \x -> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where

  pure ::
    a
    -> List a

  pure = (:. Nil)

  (<*>) ::
    List (a -> b)
    -> List a
    -> List b

  -- \a2bs -> \as -> (<$>) (\a2b -> (<$>) a2b as) a2bs
  -- Nope

  (<*>) Nil _ = Nil
  (<*>) (f:.fs) as = (map f as) ++ (fs <*> as)
  -- This is foldRight
  -- Base case nil, otherwise recurse on the tail

  -- TODO: Try implement this with foldRight

  -- (<*>) a2bs as = flatMap (\a2b -> map a2b as) a2bs

-- | Insert into an Optional.
--
-- prop> \x -> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a

  pure = Full

  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b

  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Full f) (Full a) = Full (f a)
  
  -- (<*>) Empty _ = Empty
  -- (<*>) (Full f) oa = f <$> oa

  -- (<*>) oa2b oa = bindOptional (\a2b -> (<$>) a2b oa) oa2b

-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> \x y -> pure x y == x
instance Applicative ((->) t) where

  -- pure :: a -> ((->) t a)
  -- pure :: a -> (t -> a)
  pure :: a -> t -> a
  pure = const

  -- (<*>) ::
  --   ((->) t (a -> b))
  --   -> ((->) t a)
  --   -> ((->) t b)

  -- (<*>) ::
  --   (t -> (a -> b))
  --   -> (t -> a)
  --   -> (t -> b)

  -- (<*>) ::
  --   (t -> a -> b)
  --   -> (t -> a)
  --   -> (t -> b)

  (<*>) ::
    (t -> a -> b)
    -> (t -> a)
    -> t
    -> b

  -- (<*>) = \t2a2b -> \t2a -> \t -> t2a2b t (t2a t)
    
  (<*>) tab ta t = tab t (ta t)

{-
See https://en.wikipedia.org/wiki/SKI_combinator_calculus
-}

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
-- ExactlyOne 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  Applicative f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
lift2 k fa fb =
  k <$> fa <*> fb

{-
lift0 :: a -> f a (aka pure)
lift1 :: (a -> b) -> f a -> f b
lift2 :: (a -> b -> c) -> f a -> f b -> f c
lift3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
-}

{-

f() {
  for (xx in x) {
    if (xx === null) {
      return null
    }
    for (yy in y) {
      if (yy === null) {
        return null
      }
      return xx ++ yy
    }
  }
}

f(x, y, a) {
  return x(a) + y(a)
}

Same as lift2 (+)

E.g. lift2 (+) (+10) (*99)
-}

-- | Apply a ternary function in the environment.
-- /can be written using `lift2` and `(<*>)`./
--
-- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
-- ExactlyOne 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Applicative f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 k fa fb fc =
  let x = lift2 k fa fb
  in x <*> fc
{-
k = (a -> b -> c -> d)
fa = f a
fb = f b
fc = f c
x = f (c -> d)
-}

-- | Apply a quaternary function in the environment.
-- /can be written using `lift3` and `(<*>)`./
--
-- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
-- ExactlyOne 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Applicative f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 k fa fb fc fd =
  let x = lift3 k fa fb fc
  in x <*> fd
  
-- | Apply a nullary function in the environment.
lift0 ::
  Applicative f =>
  a
  -> f a
lift0 =
  pure

-- | Apply a unary function in the environment.
-- /can be written using `lift0` and `(<*>)`./
--
-- >>> lift1 (+1) (ExactlyOne 2)
-- ExactlyOne 3
--
-- >>> lift1 (+1) Nil
-- []
--
-- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
lift1 ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
lift1 =
  (<$>)

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> \a b c x y z -> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> \x y -> Full x *> Full y == Full y
(*>) ::
  Applicative f =>
  f a
  -> f b
  -> f b

-- (*>) fa fb = lift2 (\_ b -> b) fa fb
-- (*>) = lift2 (\_ b -> b)
-- (*>) = lift2 (flip const)
(*>) = lift2 (const id)

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> \x y z a b c -> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> \x y -> Full x <* Full y == Full x
(<*) ::
  Applicative f =>
  f b
  -> f a
  -> f b

-- (<*) fb fa = lift2 const fb fa
(<*) = lift2 const

{-
Notice:
  <*    Ignore the right element
  *>    Ignore the left element
-}

{-
Why are we doing this?
So we don't have to write the same abstractions over and over again.
-}

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)

-- sequence Nil = pure Nil
-- sequence (h:.t) = lift2 (:.) h (sequence t)

{-
h           :: f a
t           :: List (f a)
sequence t  :: f (List a)
???         :: f (List a)
-}

-- sequence = foldRight (\fa fas -> lift2 (\a as -> a :. as) fa fas) (pure Nil)
sequence = foldRight (lift2 (:.)) (pure Nil)

{-
Applicative discovered in the 2000's
-}

{-
List (f        a) -> f        (List a)
List (Optional a) -> Optional (List a)
List (List     a) -> List     (List a)
List (x     -> a) -> x ->      List a

TODO: Write examples of these using sequence

See: https://qfpl.io/posts/fp-cheat-sheet/
-}


-- | Replicate an effect a given number of times.
--
-- /Tip:/ Use `Course.List#replicate`.
--
-- >>> replicateA 4 (ExactlyOne "hi")
-- ExactlyOne ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA =
  error "todo: Course.Applicative#replicateA"

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering =
  error "todo: Course.Applicative#filtering"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist

(>>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
