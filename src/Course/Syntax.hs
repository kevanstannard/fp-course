{-# OPTIONS_GHC -Wall #-}
{- ^^ Turn on all warnings -}

{-
To run this file, start ghci with:

  ghci -ignore-dot-ghci

-}

x' :: Integer
x' = 3

f :: Integer -> Integer
f r = r + 10

{-
Integer is a concrete data type
s is polymorphic
-}
g :: (Integer -> s) -> s
g q = q 99

{-

* Functions are values
* Think of them in the same way as simple values such as 123

```
> g (\i -> i + 88)
187
```
-}

{-
`->` is right associative.

This means that:
  Integer -> Integer -> Integer

  Is actually this:
  Integer -> (Integer -> Integer)

Example usage:

```
> g (h 3)
204
```
-}

{-
`\` is an approximation of the lambda symbol
-}

gg :: (Integer -> s) -> s
gg = \q -> q 99


h' :: Integer -> (Integer -> Integer)
h' p q = (p + q) * 2

{-
Important: Functions in Haskel always take 1 argument
-}

{-
Infix notation:

> 77 `h` 88
165
-}

{-
Prefix notation:

> (+) 77 88
165
-}

blah :: a -> a
blah j = j

{-
Puzzle: What does this do?
This is the only function that can possibly implement this type.
You could say the type is "once inhabited".
I.e. if multiple functions can implement a type,
then it is inhabited by multiple functions.
-}

blahblah :: a -> b -> a
blahblah a _ = a

{-
Polymorphic types start with a lowercase character.
-}

{-
In ghci, use `:info` for info about a type
-}

-- 
data Boolean = T | F
  deriving (Show, Eq)

{-
Note there is a built in `Bool` type. This type is different.
-}

data Shape =
  Circle Integer
  | Rectangle Integer Integer 
  | Integer ::: Integer -- Rectangle, nice!
  | Triangle Integer Integer Integer
  deriving (Show, Eq)

{-
Rectangle is not a type
Rectangle is a data contructor
-}

piInt :: Integer
piInt = 3

-- Algebraic Data Type (ADT) simple constructors with values
perimeter :: Shape -> Integer
perimeter sh =
  case sh of
    Circle r -> 2 * piInt * r -- C=2πr
    w ::: h -> (w + h) * 2
    Rectangle w h -> (w + h) * 2
    Triangle a b c -> a + b + c

perimeteragain :: Shape -> Integer
perimeteragain (Circle r) = 2 * piInt * r
perimeteragain (w ::: h) = (w + h) * 2
perimeteragain (Rectangle w h) = (w + h) * 2
perimeteragain (Triangle a b c) = a + b + c

-- Polymorphic type
data Three a = Threeee a a a
  deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (Threeee x y z) = x * y * z

multiplyagain :: Three Integer -> Integer
multiplyagain is = case is of
  Threeee x y z -> x * y * z

multiplyagain2 :: Three Integer -> Integer
multiplyagain2 = \is -> case is of
  Threeee x y z -> x * y * z

{-
Define all natural numbers
-}
data NaturalNumber =
  Zero
  | Successor NaturalNumber
  deriving (Show)

one :: NaturalNumber
one = Successor Zero

two :: NaturalNumber
two = (Successor (Successor Zero)

three :: NaturalNumber
three = Successor (Successor (Successor Zero))

{-
Successor is like "+1"
We remove the +1 from y and move the +1 to the x
-}
add :: NaturalNumber -> NaturalNumber -> NaturalNumber
add Zero x = x
add (Successor y) x = add y (Successor x)

add' :: NaturalNumber -> NaturalNumber -> NaturalNumber
add' Zero x = x
add' (Successor y) x = Successor (add x y)

infinity :: NaturalNumber
infinity = Successor infinity

mult :: NaturalNumber -> NaturalNumber -> NaturalNumber
mult Zero _ = Zero
mult (Successor r) y = add y (mult r y)

{-
Extra

exp :: NaturalNumber -> NaturalNumber -> NaturalNumber
greaterthan :: NaturalNumber -> NaturalNumber -> Bool

-}

