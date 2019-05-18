{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

{-
For something to be a Monad, it already has to be an Applicative.

Reminder:

  (<$>)   ::   (a -> b) -> f a -> f b     -- fmap   // Covariant Functor
  (<*>)   :: f (a -> b) -> f a -> f b     -- apply  // Applicative Functor
  (=<<)   :: (a -> f b) -> f a -> f b     -- bind   // Monadic Functor

  There are more types of Functors

  Functors go from f a -> f b

-}

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) =
    bindExactlyOne

{-
TIP:

  It's useful to use pattern matching when implementing instances
  (such as the implementation of bindExactlyOne).

-}
   
-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) =
    flatMap       -- flatMap is bind

{-
How to solve?:

Look for the same (or similar) type pattern in the List module.
In this case, the type pattern is the same.

-}

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) =
    bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119

{-
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b))
    -> ((->) t a)
    -> ((->) t b)
  (=<<) =
    error "todo: Course.Monad (=<<)#instance ((->) t)"
-}

instance Monad ((->) t) where
  (=<<) ::
    (a -> t -> b)
    -> (t -> a)
    -> (t -> b)

  -- Me
  -- (=<<) f a =
  --   (flip f) <*> a

  -- McKenner
  (=<<) atb ta t =
    atb (ta t) t

{-
How to solve?:

Look for a similar pattern in previous modules.
Notice that the Functor module has a reader implementation.
Next, notice that the only difference is the order of the arguments of the function,
so we just need to flip them.
-}



-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b

-- (<**>) fab fa =
--   (=<<) (\ab -> ab <$> fa) fab

-- (<**>) fab fa =
--   (=<<) (<$> fa) fab

(<**>) fab fa =
  (<$> fa) =<< fab
  
{-
Notice that this is the pattern for apply.

This is to show that for  anything that's a Monad, we can derive the applicative.

i.e. Don't use <*> apply to solve this.

How to solve?:

Notice that the bind operator can be used to "unwrap" values inside a type.

Example:

  Consider fab :: f (a -> b), then using bind

    (=<<) (\ab -> _todo) fab

  Here ab :: (a -> b)
  
  We unwrap fab and extract the (a -> b) from the type.

  Once we've unwrapped the value, then we can use functions like fmap :: (a -> b) -> f a -> f b
  to apply the function to a type.

-}
 
infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad f =>
  f (f a)
  -> f a

-- join ffa =
--   id =<< ffa

join =
  (id =<<)

{-
How to solve?:

  First, since we're doing a Monad exercise, we assume that bind should be used.
  Then just solve for types.

More examples:

  >> join (ExactlyOne (ExactlyOne 3))
  ExactlyOne 3

  >> join Empty
  Empty

-}


-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad f =>
  f a
  -> (a -> f b)
  -> f b

(>>=) fa afb =
  join (afb <$> fa)

{-
How to solve?:

  ???

TODO:

  Work out some more examples of calling this function.

  >> (Full 2) >>= (\n -> Full (n :. n :. n :. Nil))
  Full [2,2,2]

  >> (ExactlyOne 3) >>= (\n -> ExactlyOne (n*n))
  ExactlyOne 9

  >> (Full (-1)) >>= (\n -> if n < 0 then Empty else (Full n))
  Empty

  >> (Full 3) >>= pure . (2*)
  Full 6

  >> (Full 3) >>= pure . (:. Nil)
  Full [3]

  >> ((1  :. Nil) :. (2 :. 3 :. Nil) :. Nil) >>= (:. Nil) . length
  [1,2]

-}
  
infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c

(<=<) bfc afb a =
  bfc =<< afb a
  
{-
How to solve?:

  1. Pass a into (a -> f b) = f b
  2. Use >>= to pass f b into (b -> f c)
  3. Return the result, fc

-}
    
infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
