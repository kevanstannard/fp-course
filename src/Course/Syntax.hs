import Prelude
import Data.String

module Course.Introduction where

{-
Introducing Haskell
This section is a guide for the instructor to introduce Haskell syntax. Each of
these points should be covered before attempting the exercises.
-}

-- * values, assignment

{-
Assigning values to identifiers
-}

x = "Hello"
y = 2
z = True


-- * type signatures `::` reads as *has the type*
--   * The `->` in a type signature is *right-associative*

x :: String   -- x has the type String
x = "Hello"

y :: Int
y = 2

z :: Bool
z = True

w :: [Int]  -- w has the type list of Int
w = [1, 2, 3]


-- * functions are values
-- * functions take arguments
--   * functions take *only one argument* but we approximate without spoken
--     language
--   * functions can be declared inline using *lambda expressions*
--   * the `\` symbol in a lambda expression denotes a Greek lambda

f :: Int -> Int
f = \a -> a + 1
{-
Takes in an argument a.
The value of computing this function is a + 1
-}

{-
Two ways of writing a function.
These are equivalent.
-}
f :: Int -> Int
f = \a -> a + 1
f a = a + 1

{-
A function with two arguments.
-}
g :: Int -> Int -> Int
g a b = a + b
-- or
g = \a b -> a + b

{-
Another way of writing the type.
g is a function that takes an Int and returns a function that takes an Int.
-}
g :: Int -> (Int -> Int)

{-
Very important: All functions in Haskell take one argument, always.
-}




-- * operators, beginning with non-alpha character, are in infix position by
--   default
--   * use in prefix position by surrounding with *(parentheses)*

{-
Using a function called "plus"
-}
h :: Int
h = (+) 1 2     -- prefix notation
h = 1 + 2       -- infix notation


-- * regular identifiers, beginning with alpha character, are in prefix position by
--   default
--   * use in infix position by surrounding with ``backticks``

-- Suppose we have
g :: Int -> Int -> Int
g a b = a + b

-- Calling a function
h :: Int
h = g 1 2

-- or
h :: Int
h = 1 `g` 2     -- Infix on a normal identified


-- * polymorphism
--   * type variables *always* start with a lower-case character

-- Notice lowercase a here
-- lowercase means polymorphic
-- Notice previous functions always use capitals
i :: a -> a
i x = x

-- When you start an identifier with lowercase in a type
-- it means it is polymorphic.


-- * data types, declared using the `data` keyword
--   * following the `data` keyword is the *data type name*
--   * following the data type name are zero of more type variables
--   * then `=` sign
--   * data types have zero or more constructors
--     * data type constructors start with an upper-case character, or colon `(:)`
--   * following each constructor is a list of zero or more *constructor arguments*
--   * between each constructor is a pipe symbol `(|)`
--   * the `deriving` keyword gives us default implementations for some functions
--     on that data type
--   * when constructors appear on the left side of `=` we are *pattern-matching*
--   * when constructors appear on the right side of `=` we are *constructing*


data Person = MakePerson String Int
-- LHS is the type name
-- RHS is the data constructor
-- We use MakePerson to construct a Person

brian :: Person
brian = MakePerson "Brian" 27

data Z = X String | Y
  derive (Show)
-- This is called a "sum" type

j :: z
j = X "Testing"


data Bool' = True' | False'
-- identifiers can have ticks in them
-- Usually used to escape variable bindings

k :: Z -> String
k (X s) = s     -- Pattern matching
k Y = "y"


l :: Z -> String
l z = case z of
  X s -> s
  Y -> "y"


-- * type-classes

instance Eq Z where
  X a == X b = a == b
  Y == Y = True
  _ == _ = False


class ToString a where
  toString :: a -> String

instance ToString Int where
  toString = show

instance ToString Bool where
  toString True = "True"
  toString False = "False"

