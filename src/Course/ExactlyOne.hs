{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

data ExactlyOne a = ExactlyOne a
  deriving (Eq, Show)
{-
Purpose:
* Type syntax

Notes:
* Declares a type constructor ExactlyOne.
* Declares a data constructor ExactlyOne.
* Note that a data constructor is a *function* that takes an argument.
* This provides a simple wrapper for a value.
* `deriving (Eq, Show)` allows us to check if
  two ExactlyOne values are equal, and display ExactlyOne values.

Examples:

>> ExactlyOne 3
ExactlyOne 3

>> ExactlyOne "Hello"
ExactlyOne "Hello"

>> (ExactlyOne 3) == (ExactlyOne 0)
false
-}

runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a
{-
Purpose:
* Function syntax

Notes:
* Takes one argument of type ExactlyOne and returns it's contained value

Examples:

>> runExactlyOne (ExactlyOne 3)
3

>> runExactlyOne (ExactlyOne "Hello")
"Hello"

Exercises:

Ex 1.
Write an expression that has an ExactlyOne value _inside_ an ExactlyOne,
and use `runExactlyOne` to extract the inner ExactlyOne.

>> runExactlyOne (ExactlyOne (ExactlyOne 123))
ExactlyOne 123

Ex 2.
What does the following expression return? Explain.
>> (runExactlyOne . ExactlyOne) 123
123

Ex 3.
What does the following expression return? Explain.
>> (ExactlyOne . runExactlyOne . ExactlyOne) 123
ExactlyOne 123

Ex 4.
What does the following expression return? Explain.
>> (runExactlyOne . runExactlyOne . ExactlyOne . ExactlyOne) "Hello"
"Hello"

-}


mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a) = ExactlyOne (f a)
{-
Notes:
* Applies a function to the value inside an `ExactlyOne`.
* Notice that it can transform the type of the value.

Cheatsheet:
* This pattern and its name is important to remember.
  
  fmap :: (a -> b) -> f a -> f b

Examples:

>> mapExactlyOne (\x -> x + 10) (ExactlyOne 3)
ExactlyOne 13

>> mapExactlyOne (+10) (ExactlyOne 3)
ExactlyOne 13

>> mapExactlyOne (\a -> if a then "Yes" else "No") (ExactlyOne True)
ExactlyOne "Yes"

Exercises:

Ex 1.
Write an expression that calls `mapExactlyOne` with a function that does not change the value.

>> mapExactlyOne (\a -> a) (ExactlyOne 3)
ExactlyOne 3

Ex 2.
Write a `mapExactlyOne` expression using the `id` function so that it does not change the value.

>> mapExactlyOne (id) (ExactlyOne 3)
ExactlyOne 3

Ex 3.
Suppose you have the following function wrapped in ExactlyOne:
   
  ExactlyOne (\a -> a * a)
   
Write an expression that
(a) passes this into runExactlyOne, then
(b) passes it's result into mapExactlyOne.

>> mapExactlyOne (runExactlyOne (ExactlyOne (\a -> a * a))) (ExactlyOne 3)
ExactlyOne 9

-}

bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a
{-
Notes:
* Notice the difference in pattern compared to fmap.

Cheatsheet:
* This pattern and its name is important to remember.

  bind :: (a -> f b) -> f a -> f b

Examples:

>> bindExactlyOne (\a -> ExactlyOne (a*a)) (ExactlyOne 3)
ExactlyOne 9

>> bindExactlyOne (\a -> if (a<0) then (ExactlyOne "-") else (ExactlyOne "+")) (ExactlyOne (-3))
ExactlyOne "-"

Exercises:

Ex 1.
Write an expression that calls `bindExactlyOne` with a function that adds 1 to it's argument.

>> bindExactlyOne (\a -> ExactlyOne (a + 1)) (ExactlyOne 3)
ExactlyOne 4

Ex 2.
Write an expression that calls bindExactlyOne and returns the value unchanged.

>> bindExactlyOne (\a -> ExactlyOne a) (ExactlyOne 3)
ExactlyOne 3

>> bindExactlyOne (ExactlyOne) (ExactlyOne 3)
ExactlyOne 3

Ex 3.
What does this expression return? Explain.
>> ((mapExactlyOne id) . (bindExactlyOne ExactlyOne)) (ExactlyOne 3)
-}

instance P.Functor ExactlyOne where
  fmap =
    M.liftM

instance A.Applicative ExactlyOne where
  (<*>) =
    M.ap
  pure =
    ExactlyOne

instance P.Monad ExactlyOne where
  (>>=) =
    flip bindExactlyOne
  return =
    ExactlyOne

