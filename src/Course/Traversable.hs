{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

{-
Traverse is similar to `sequence` (from Applicative) but is more general.

Notice that a traversible is a Functor, so supports fmap <$>
-}

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

{-
Examples:

  >> traverse (\a -> ExactlyOne a) (1 :. 2 :. 3 :. Nil)
  ExactlyOne [1,2,3]

  >> traverse (\a -> Full (a*2)) (1 :. 2 :. 3 :. Nil)
  Full [2,4,6]

  >> traverse (\f -> ExactlyOne (f 2)) ((*1) :. (*2) :. (*3) :. Nil)
  ExactlyOne [2,4,6]

-}

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)

  traverse fn (ExactlyOne a) =
    ExactlyOne <$> (fn a)

{-
Examples:

  >> traverse (\a -> Full (a*2)) (ExactlyOne 2)
  Full (ExactlyOne 4)


-}
  
instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)

  traverse _ Empty =
    pure (Empty)

  traverse fn (Full a) =
    Full <$> (fn a)

{-
Tips:

  Use pure to create a "default" f in the Empty case

  Use pattern matching.

More examples of traverse

  >> traverse (ExactlyOne) (Full 2)
  ExactlyOne (Full 2)

  >> traverse (\n -> ExactlyOne (n*2)) (Full 2)
  ExactlyOne (Full 4)

  >> traverse (\n -> ExactlyOne (n*2)) (Empty)
  ExactlyOne Empty

  >> traverse (\n -> (n :. n*n :. n*n*n :. Nil)) (ExactlyOne 3)
  [ExactlyOne 3,ExactlyOne 9,ExactlyOne 27]

  >> traverse (\n -> ExactlyOne (n*2)) (1 :. 2 :. 3 :. Nil)
  ExactlyOne [2,4,6]

Simple explanation of traverse:

  It applies a function to a value in a type (or values in a type, in the case of lists),
  then lifts the final result into a new type.

-}
  
-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)

-- sequenceA tfa =
--   traverse id tfa

sequenceA =
  traverse id
  
{-
From the sequenceA type, we know that t is traversable
so guess tha we need to apply traverse.

Use holes to determine the type of the traverse function.

-}

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose

  traverse ::
    Applicative h =>
    (a -> h b)
    -> Compose f g a
    -> h (Compose f g b)

-- This produces the correct types, but has an infinite loop
-- traverse fn fga =
--   let
--     x = fn <$> fga    -- x :: Compose f g (h b)
--     y = sequenceA x   -- y :: h (Compose f g b)
--   in y

-- traverse f (Compose fga) =
--   let
--     x = traverse (\ga -> traverse f ga) fga   -- x :: h (f (g b))
--     y = Compose <$> x                         -- y :: h (Compose f g b)
--   in y

{-
Examples:

>> traverse (\n -> ExactlyOne (n*2)) (Compose (Full (ExactlyOne 2)))
ExactlyOne (Compose (Full (ExactlyOne 4)))


How to solve?:

We have the following arguments:

(a -> h b)
Compose f g a

And we want to produce:

h (Compose f g b)

Let's consider what `traverse` means.

`traverse` takes a wrapped value, such as `t a` (where a is the value)
and applies a function to that value, then returns `t b` but wrapped in
an /additional/ type, such as `f (t b)`.

In the case of Compose we have this:

Compose (f (g a))
\-----------/ ^
    ^       |
    |       +--- Value
    |
    +--- Wrapper

So from the perspective of `traverse` our `t` is `Compose (f (g))`

Also, consider the type of `traverse`:

traverse :: (a -> h b) -> Compose f g a -> h (Compose f g b)

Notice that the function is not wrapped in a type constructor,
so this has similarities to both fmap <$> and bind =<<, in that
they all /unwrap/ values:

fmap  <*> :: (a -> b)   -> f a -> f b
bind  =<< :: (a -> f b) -> f a -> f b

Next, just a reminder on how to read `Compose (f (g a))`

This means:

-> We have a type constructor `Compose`, containing
-> a type constructor `f`, containing
-> a type constructor `g`, containing
-> a value `a`

Back to the original question:

We have the following arguments:

(a -> h b)
Compose f g a

And we want to produce:

h (Compose f g b)

-}

-- [1] Start with an error
-- traverse =
--   error ""

-- [2] unwrap `Compose fga` using pattern matching
-- traverse f (Compose fga)=
--   _todo

-- [3] unwrap `fga` using traverse to get access to `g a`
-- traverse f (Compose fga)=
--   let
--     x = traverse (\ga -> _todo1) fga
--   in _todo0

-- [4] unwrap `ga` using traverse to get access to `a`
-- traverse f (Compose fga)=
--   let
--     x = traverse (\ga -> traverse (\a -> _todo1) ga) fga
--   in _todo0

-- [5] Apply our function `f :: (a -> h b)`
-- traverse f (Compose fga)=
--   let
--     x = traverse (\ga -> traverse (\a -> f a) ga) fga     -- x :: h (f (g b))
--   in _todo0

{-
At this point we have `x :: h (f (g b))`

How?

Consider the two `traverse` steps (pseudo code):

[1] Note that `(\a -> f a)` === (a -> h b), then applying the first inner traverse we get:

traverse (\a -> h b) (ga) :: h (g b)

[2] Then applying the second traverse:

traverse (\ga -> h gb) fga :: h (f (g b))

Notice how the `h` is kept as the outer type.

-} 

-- [6] We now need to lift the `f (g b)` into Compose
-- traverse f (Compose fga)=
--   let
--     x = traverse (\ga -> traverse (\a -> f a) ga) fga     -- x :: h (f (g b))
--     y = Compose <$> x                                     -- h (Compose f g b)
--   in _todo0

-- [7] Now we have the solution
-- traverse f (Compose fga)=
--   let
--     x = traverse (\ga -> traverse (\a -> f a) ga) fga     -- x :: h (f (g b))
--     y = Compose <$> x                                     -- h (Compose f g b)
--   in y

-- [8] Refactoring
-- traverse f (Compose fga)=
--   Compose <$> traverse (traverse f) fga

-- [9] More refactoring
  traverse f (Compose fga)=
    Compose <$> (traverse . traverse) f fga

{-
Note:

The `traverse` function actually composes

-}





-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

{-
Examples:

>> Product (ExactlyOne 3) (Full 2)
Product (ExactlyOne 3) (Full 2)

>> Product (ExactlyOne (1 :. 2 :. Nil)) (Full (3 :. Nil))
Product (ExactlyOne [1,2]) (Full [3])

-}



instance (Functor f, Functor g) =>
  Functor (Product f g) where

  -- Implement the (<$>) function for a Functor instance for Product

  {-
  Examples:

    >> (\n -> n * 2) <$> (Product (ExactlyOne 2) (ExactlyOne 3))
    Product (ExactlyOne 4) (ExactlyOne 6)

    >> (\n -> n * 2) <$> (Product (2 :. Nil) (Full 3))
    Product [4] (Full 6)

  Notes:

    This is in the Traversible module, so it may use `traverse`.
  
  -}

  (<$>) ::
    (a -> b)
    -> Product f g a
    -> Product f g b

  -- [1] Notice that the `Product f g a` type has two arguments, `f a` and `g a`
  -- Use pattern matching to get access to the two arguments
  -- (<$>) f (Product fa ga) =
  --   _todo

  -- We need to unwrap both values
  -- Simply apply the function and wrap them again in `Product`
  (<$>) f (Product fa ga) =
    let
      fb = f <$> fa
      gb = f <$> ga
    in Product fb gb

  {-
  Note: Interesting that we did not need to use `traverse`
  -}






instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where

  -- Implement the traverse function for a Traversable instance for Product
  {-
  Examples:

    >> traverse (\n -> ExactlyOne (n*2)) (Product (Full 1) (Full 2))
    ExactlyOne (Product (Full 24)) (Product (Full 4))

    >> traverse (\n -> ExactlyOne (n*2)) (Product (1 :. 2 :. Nil) (Full 10))
    ExactlyOne (Product [2,4] (Full 20))

  -}

  -- Use a _todo hole to determine the type signature
  -- traverse =
  --   _todo

  {-
  This produces:

    _todo :: (a -> f1 b) -> Product f g a -> f1 (Product f g b)
  
  -}
  
  traverse ::
    Applicative h =>
    (a -> h b)
    -> Product f g a
    -> h (Product f g b)

  -- Use pattern matching to get access to the Product values
  -- Then apply traverse to the product values
  -- traverse f (Product fa ga) =
  --   let
  --     hfb = traverse f fa     -- h (f b)
  --     hgb = traverse f ga     -- h (g b)
  --   in _todo

  {-
  We now have the following:
    h (f b)
    h (g b)

  And we want
    h (Product (f b) (g b))

  This is a perfect candidate for our `lift2` function.
  -}

  -- traverse f (Product fa ga) =
  --   let
  --     hfb = traverse f fa     -- h (f b)
  --     hgb = traverse f ga     -- h (g b)
  --   in lift2 (\fb gb -> (Product fb gb)) hfb hgb

  -- Refactor
  traverse f (Product fa ga) =
    lift2 Product (traverse f fa) (traverse f ga )
  






-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)                               -- InjectLeft
  | InR (g a) deriving (Show, Eq)         -- InjectRight

{-
Examples:

  >> InL (ExactlyOne 1) :: Coproduct ExactlyOne Optional Int
  InL (ExactlyOne 1)

-}

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where

  -- Implement the (<$>) function for a Functor instance for Coproduct
  {-
  Work out a type for fmap

    _todo :: (a -> b) -> Coproduct f g a -> Coproduct f g b

  -}
  -- (<$>) =
  --   _todo


  (<$>) ::
    (a -> b)
    -> Coproduct f g a
    -> Coproduct f g b

  -- (<$>) f (InL fa) =
  --   let
  --     x = f <$> fa
  --     y = InL x
  --   in y

  -- (<$>) f (InR ga) =
  --   let
  --     x = f <$> ga
  --     y = InR x
  --   in y

  -- Refactor
  (<$>) f (InL fa) = InL (f <$> fa)
  (<$>) f (InR ga) = InR (f <$> ga)

  {-
  Examples:

    Notice how we need to specify the type on the command line

    >> (+1) <$> (InL (ExactlyOne 1)) :: Coproduct ExactlyOne ExactlyOne Int
    InL (ExactlyOne 2)

    >> (+1) <$> (InR (Full 1)) :: Coproduct ExactlyOne Optional Int
    InR (Full 2)

  -}

  {-
  These two are quite similar, could they be done without pattern matching?
  -}





instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where

  -- Implement the traverse function for a Traversable instance for Coproduct
  -- Determine the type:

  traverse ::
    Applicative h =>
    (a -> h b)
    -> Coproduct f g a          --    InL (f a)  |    InR (g a)
    -> h (Coproduct f g b)      -- h (InL (f b)) | h (InR (g b))

  -- traverse f (InL fa) =
  --   let
  --     x = traverse f fa         -- x :: h (f b)
  --     y = InL <$> x             -- y :: forall (g :: * -> *). h (Coproduct f g b)
  --   in y

  -- Refactor
  traverse f (InL fa) = InL <$> traverse f fa
  traverse f (InR ga) = InR <$> traverse f ga

  {-
    Examples:

    >> traverse (\n -> ExactlyOne (n*2)) (InL (Full 2)) :: ExactlyOne (Coproduct Optional Optional Int)
    ExactlyOne (InL (Full 4))

  -}
  