{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)
{-
Declares a type that is a container for two composed functions.

Important:
* Don't think of (f (g a)) like mathematics,
  instead think of it as:
  * Type constuctor f with an argument `g a`
  * Type constructor g with an argument `a`
* Examples:
  Compose (Full (Full 3))           -- :: Compose (Optional (Optional Int))
  Compose (Full (1 :. 2 :. Nil))    -- :: Compose (Optonal (List Int))
  Compose (Full 1 :. Full 2 :. Nil) -- :: Compose (List (Optional Int))
-}

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where

  (<$>) ::
    (a -> b)
    -> Compose f g a
    -> Compose f g b
  
  (<$>) ab (Compose fga) =
    let fgb = (ab <$>) <$> fga
    in Compose fgb
{-
Example:

>> (+1) <$> (Compose (Full (Full 3)))
Compose (Full (Full 4))

-}

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

  -- Implement the pure function for an Applicative instance for Compose
  pure =
    -- Compose (pure (pure a))
    Compose . pure . pure
  {-
  Notes:
  * Just use the pure function wherever a type should be
    and Haskell should just work out the value.
  -}

  -- Implement the (<*>) function for an Applicative instance for Compose
  -- (<*>) fgab fga =
  --   lift2 id fgab fga

  (<*>) ::
    Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b

  (<*>) (Compose fgab) (Compose fga) =
    Compose (lift2 (<*>) fgab fga)
  
{-
Example:

>> Compose (Full (Full (+1))) <*> Compose (Full (Full 1))
Compose (Full (Full 2))
-}

    
instance (Monad f, Monad g) =>
  Monad (Compose f g) where

  -- Implement the (=<<) function for a Monad instance for Compose

  (=<<) ::
    (a -> Compose f g b)
    -> Compose f g a
    -> Compose f g b


  (=<<) f (Compose fga) =
    error "impossible"

{-
Note:
When we unwrap the Compose type, it will get wrapped again.
Suppose we unwrap `Compose f g a`, then call the function `f`
and pass it `a`, then we get a `Compose f g b`,
but this gets automatically re-wrapped again,
which gives us:
Compose f g (Compose f g b)

So bind is not available for Compose.
-}
