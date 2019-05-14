{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Validation where

import qualified Prelude as P(String)
import Course.Core

--  class Validation<A> {
--    Validation(String error) {} // Error
--    Validation(A value) {} // Value
--  }

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap, either)
-- >>> instance Arbitrary a => Arbitrary (Validation a) where arbitrary = P.fmap (P.either Error Value) arbitrary
data Validation a = Error Err | Value a
  deriving (Eq, Show)
{-
Purpose:
* Type Syntax

Notes:
* Declares a type constructor `Validation` that has a type argument `a`
* Declares two data constructors

Examples:

>> Value "Hello"

>> Value 3

>> Error "Message"
-}

type Err = P.String
{-
Purpose:
* Type syntax

Notes:
* Declares an alias for a String

-}

-- | Returns whether or not the given validation is an error.
--
-- >>> isError (Error "message")
-- True
--
-- >>> isError (Value 7)
-- False
--
-- prop> \x -> isError x /= isValue x
isError :: Validation a -> Bool
isError (Error _) = True
isError (Value _) = False
{-
Notes:
* Uses pattern matching on the Validation values.
-}

-- | Returns whether or not the given validation is a value.
--
-- >>> isValue (Error "message")
-- False
--
-- >>> isValue (Value 7)
-- True
--
-- prop> \x -> isValue x /= isError x
isValue :: Validation a -> Bool
isValue = not . isError
{-
Notes:
* Equivalent to:
  ```
  isValue a = not (isError a)
  ```
-}

-- | Maps a function on a validation's value side.
--
-- >>> mapValidation (+10) (Error "message")
-- Error "message"
--
-- >>> mapValidation (+10) (Value 7)
-- Value 17
--
-- prop> \x -> mapValidation id x == x
mapValidation :: (a -> b) -> Validation a -> Validation b
mapValidation _ (Error s) = Error s
mapValidation f (Value a) = Value (f a)
{-
Notes:
* Notice that the type matches the fmap pattern (a -> b) -> f a -> f b
* Notice that Error values are left unchanged and Value values have the function applied.
-}

-- | Binds a function on a validation's value side to a new validation.
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
-- Error "message"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
-- Error "odd"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
-- Value 18
--
-- prop> \x -> bindValidation Value x == x
bindValidation :: (a -> Validation b) -> Validation a -> Validation b
bindValidation _ (Error s) = Error s
bindValidation f (Value a) = f a
{-
Notes:
* Notice that the type matches the bind pattern: (a -> f b) -> f a -> f b
-}

-- | Returns a validation's value side or the given default if it is an error.
--
-- >>> valueOr (Error "message") 3
-- 3
--
-- >>> valueOr (Value 7) 3
-- 7
--
-- prop> \x -> isValue x || valueOr x n == n
valueOr :: Validation a -> a -> a
valueOr (Error _) a = a
valueOr (Value a) _ = a

-- | Returns a validation's error side or the given default if it is a value.
--
-- >>> errorOr (Error "message") "q"
-- "message"
--
-- >>> errorOr (Value 7) "q"
-- "q"
--
-- prop> \x -> isError x || errorOr x e == e
errorOr :: Validation a -> Err -> Err
errorOr (Error e) _ = e
errorOr (Value _) a = a

valueValidation :: a -> Validation a
valueValidation = Value
