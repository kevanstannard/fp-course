{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b)
  -> Optional a
  -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) ::
  Optional a
  -> a
  -> a
(??) (Full a) _ = a
(??) Empty a = a

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a
  -> Optional a
  -> Optional a
(<+>) (Full a) _ = Full a
(<+>) Empty a = a

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional ::
  (a -> b)
  -> b
  -> Optional a
  -> b
optional _ b Empty = b
optional f _ (Full a) = f a


applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

{-

Recall:

  bindOptional :: (a -> Optional b) -> Optional a -> Optional b
  mapOptional :: (a -> b) -> Optional a -> Optional b

  Here, bindOptional is unwrapping the Optional (a -> b) and providing the (a -> b)
  to the inner function.

Examples:

  >> applyOptional Empty Empty
  Empty

  >> applyOptional Empty (Full 2)
  Empty

  >> applyOptional (Full (+1)) Empty
  Empty

  >> applyOptional (Full (+1)) (Full 3)
  Full 4

Note:
  * APPLY is a very important pattern to recognise
  * In order to get INTUITION about what this is doing it's important to be able to
    INTERNALISE the patterns of map and bind.

Consider the arguments:
  [1] Optional (a -> b)
  [2] Optional a

  We need to UNWRAP [1] to get access to the function
  We need to UNWRAP [2] to get access to the value
  Once we've done that, then we can pass the value into the function
  THEN we need to wrap the result in an Optional again

Consider this:

  Suppose applyOptional was defined as follows:
    applyOptional :: (a -> b) -> Optional a -> Optional b

  How would you implement it?
  
  This looks EXACTLY like MAP. So a verbose implementation might be:

    applyOptional' :: (a -> b) -> Optional a -> Optional b
    applyOptional' f a = mapOptional f a

  So this works, but we just need access to the function to do this.

  We can use BIND to unwrap it for us
  PLUS at the same time, get it to return the Optional b we're after.

  So we can solve TWO problems with one function.

Let's consider the solution provided.

f is an Optional
  * it might have the value Empty
  * it might have the value Full (a -> b)
  * we name it "f" to mean its "intention" is to hold a function
  * But keep in mind that it's an Optional

f' is a FUNCTION extracted from the Optional
  * But this is only used if the Optional is a "Full"

Remember this:

  BIND takes two arguments
    [1] a -> T b
    [2] T a
  
  It does the following
    [a] Extracts the value from argument [1], and
    [b] passes that value to agument [2]

-}

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

{-
What does it do?

Examples:

  >> twiceOptional (\a b -> a + b) (Full 2) (Full 3)
  Full 5

  >> twiceOptional (\a b -> a + b) (Full 2) Empty
  Empty

  >> twiceOptional (\a b -> a + b) Empty (Full 3)
  Empty

  >> twiceOptional (\a b -> a + b) Empty Empty
  Empty

Note the difference between mapOptional and applyOptional:

  applyOptional :: Optional (a -> b) -> Optional a -> Optional b
  mapOptional   ::          (a -> b) -> Optional a -> Optional b

Now, considering the defintion of mapOptional:

  mapOptional :: (a -> b) -> Optional a -> Optional b

Which maps an Optional value onto the provided function.

And consider the type of f:

  f :: (a -> b -> c)

mapOptional takes the value of (Optional a) and passes it to the function
providing the result:

  Optional (b -> c)

We then pass this to as the FIRST argument to applyOptional.

Recall the definition of applyOptional:

  applyOptional :: Optional (b -> c) -> Optional b -> Optional c

Now, the original Optional b is passed as the SECOND argument,
which produces the final result Optional C
  
-}

-- Another way of writing this
twiceOptional' :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional' f a b = applyOptional (mapOptional f a) b

{-
CHALLENGE

applyTwiceOptional :: Optional (a -> b -> c) -> Optional a -> Optional b -> Optional c

>> applyTwiceOptional (Full (+)) (Full 2) (Full 3)
Full 5

-}

applyTwiceOptional :: Optional (a -> b -> c) -> Optional a -> Optional b -> Optional c
applyTwiceOptional f a b = applyOptional (applyOptional f a) b


-- Better
applyTwiceOptional' :: Optional (a -> b -> c) -> Optional a -> Optional b -> Optional c
applyTwiceOptional' f = applyOptional . (applyOptional f)

{-
This is easy to derive because both map and apply return the SAME type,
but have a slightly different first argument.

map takes a simple function.
apply takes a function wrapped in a type.

But the rest of their behaviour is identical
-}


contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
