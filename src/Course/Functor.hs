{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)


{-
*****************************************************
*                                                   *
*   A Functor is just a way to go from f a -> f b   *
*                                                   *
*****************************************************
-}

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<$>) f =
    (ExactlyOne . f . runExactlyOne)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) =
    map

{-
Note "browse" command:

  >> :browse Course.List

map works over lists
fmap works over anything

-}

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) =
    mapOptional

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17

{-
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  (<$>) =
    error "todo: Course.Functor (<$>)#((->) t)"
-}

instance Functor ((->) t) where
  (<$>) ::
    (a -> b)      -- This is: f :: a -> b
    -> (t -> a)   -- This is: g :: t -> a
    -> (t -> b)   -- This is: f (g t) :: t -> a -> b
  -- (<$>) f g =
  --   f . g
  (<$>) =
    (.)
    
{-
Why is this called a reader?

  Perhaps because it's explicitly declared to read a "t"?

Note:

  The functor for functions is composition.

Reminder:

  Int -> Bool
  
is the same as:
  
  (->) Int Bool
  
This is a function that takes an Int and returns a Bool

Consider the Functor type. Another way of writing this:

  instance Functor f where
    (<$>) :: (a -> b) -> (f a) -> (f b)

But here, f is:

  (->) t

Which is a function that takes in a "t".

Replacing the f, we have:

  instance Functor ((->) t) where
    (<$>) :: (a -> b) -> ((->) t a) -> ((->) t b)

Which is the same as:

  instance Functor ((->) t) where
    (<$>) :: (a -> b) -> (t -> a) -> (t -> b)

-}


-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
-- (<$) a b = (<$>) (const a) b
-- (<$) a = (<$>) (const a)
(<$) = (<$>) . const

{-

Reminder:

  const :: a -> b -> a

What's happening here:

  [1] "a" is just a simple value
  [2] "f b" is b lifted into f. We DO care about f but we don't care about "b"
  [3] "f a" is the result we want.

  We essentially need to lift the "a" from [1] into the "f" from [2].

  But we don't know what "f" is.

  To modifiy a value inside "f", we can use FMAP, i.e. <$>.

  For example:

    >> (<$>) (\x -> x + 1) (Full 3)
    Full 4

  Now, suppose we want to throw away the value inside Full,
  and make it a constant value.

    >> (<$>) (\_ -> 7) (Full 3)
    Full 7

  But (\_ -> 7) is the same as (const 7), so we can write:

    >> (<$>) (const 7) (Full 3)
    Full 7

  Which is the pattern in the solution.

-}

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  (<$) ()

{-
Here we want to map a constant value onto a functor.
In this case it's just the unit value.
-}

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
