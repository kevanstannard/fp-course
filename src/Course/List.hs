{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Course.Optional
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t =
  Nil
  | t :. List t
  deriving (Eq, Ord)

{-
(:.) is called 'cons'

>> :t (:.)
(:.) :: t -> List t -> List t

Cons is right associative.
-}

-- Right-associative
infixr 5 :.

instance Show t => Show (List t) where
  show = show . hlist

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  let inf x = x :. inf (x+1)
  in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

{-
Pseudo code:

foldLeft f z list {
  var r = z
  for (e in list) {
    r = f(r, e)
  }
  return r
}

You cannot pass an infinite list to foldLeft.

*** foldLeft does a loop ***
*** foldRight does cons replacement ***

-}


-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr :: a -> List a -> a
headOr x Nil = x
headOr _ (h :. _) = h

headOr' :: a -> List a -> a
headOr' = \a -> \as ->
  case as of
    Nil -> a
    h :. _ -> h

headOr'' :: a -> List a -> a
headOr'' a as =
  case as of
    Nil -> a
    h :. _ -> h

-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product ::
  List Int
  -> Int
product Nil = 1
product (h :. t) = h * (product t)

product' ::
  List Int
  -> Int
product' = \xs ->
  case xs of
    Nil -> 1
    h :. t -> h * product' t

product'' ::
  List Int
  -> Int
product'' xs =
  case xs of
    Nil -> 1
    h :. t -> h * product'' t

product''' ::
  List Int
  -> Int
product''' =
  foldLeft (*) 1  

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> \x -> foldLeft (-) (sum x) x == 0
sum :: List Int -> Int
sum = \xs -> case xs of
  Nil -> 0
  h :. t -> h + sum t

sum' ::
  List Int
  -> Int
sum' Nil =
  0
sum' (h :. t) =
  h + sum' t

sum'' :: List Int -> Int
sum'' = foldLeft (+) 0

kanst :: a -> b -> a
kanst = \a _ -> a

campase  :: (b -> c) -> (a -> b) -> a -> c
campase = \f -> \g -> \a -> f (g a)

{-
There is a built in function (.) with this type
-}

{-
Important note about compose:

If you see the pattern

  \x -> f (g x)

Then we can just replace it with

  f . g

For example:

  \r -> const ( (1+) r )

Can be reduced to:

  const . (1+)

-}

{-
Similarly, if you see the pattern

  \x -> f x

Then we can just replace it with

  f

-}


-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> \x -> sum (map (const 1) x) == length x
length ::
  List a
  -> Int
length = \xs -> case xs of
  Nil -> 0
  (:.) _ t -> 1 + (length t)

length' ::
  List a
  -> Int
length' Nil =
  0
length' (_ :. t) =
  1 + (length' t)

length'' ::
  List a
  -> Int
length'' = foldLeft (\r _ -> 1 + r) 0

length''' ::
  List a
  -> Int
length''' = foldLeft (\r _ -> 1 + r) 0

length'''' ::
  List a
  -> Int
length'''' = foldLeft (const . (1+)) 0

-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
map ::
  (a -> b)
  -> List a
  -> List b
map =
  \f -> \xs -> case xs of
    Nil -> Nil
    (h :. t) -> (f h) :. (map f t)

map' ::
  (a -> b)
  -> List a
  -> List b
map' _ Nil =
  Nil
map' f (h :. t) =
  (f h) :. (map' f t)

  
-- bool :: a -> a -> Bool -> a
-- bool a b p = if p then b elsxe a

-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter _ Nil = Nil

-- filter p (h:.t) = if p h
--   then h :. filter p t
--   else filter p t

-- filter p (h:.t) = if p h
--   then (h:.) (filter p t)
--   else (id) (filter p t)

-- filter p (h:.t) =
--   (if p h then (h:.) else (id)) (filter p t)

filter p (h:.t) =
  bool (id) (h:.) (p h) (filter p t)

-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> \x -> headOr x (Nil ++ infinity) == 0
--
-- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> \x -> x ++ Nil == x
(++) ::
  List a
  -> List a
  -> List a

-- (++) Nil ys = ys
-- (++) (h:.t) ys = h :. (t ++ ys)

-- (++) = \x y -> foldRight (:.) y x

-- (++) = \x y -> flip (foldRight (:.)) x y

(++) = flip (foldRight (:.))

{-
Important: foldLeft does a LOOP.

foldLeft can't be used to write (++) because it won't work with infinity
-}

infixr 5 ++

{-
foldRight ::
  (a -> b -> b)
  -> b
  -> List a
  -> b

Consider:
  foldRight f  list

foldRight does this:
  Replace :. with f
  Replace Nil with z

*** foldLeft does a loop ***
*** foldRight does cons replacement ***

map f list = foldRight (\h t -> f h :. t) Nil list

This function "(\h t -> f h :. t)" contains the tails /after/ the recursion.

TODO: Solve headOr with foldRight
-}

{-
1 :. 2 :. 3 :. Nil
-}
headOr2 :: a -> List a -> a
-- headOr2 x xs = foldRight (\_ b -> b) x xs
-- headOr2 x xs = foldRight const x xs
headOr2 = foldRight const

{-
* Eta reduction is the process of reducing code into a point free form
* Point full is the opposite of point free
-}

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)
flatten ::
  List (List a)
  -> List a

-- flatten Nil = Nil
-- flatten (h:.t) = h ++ flatten t

flatten = foldRight (++) Nil
{-
Using cons replacement
- replace (:.) with (++)
- replace Nil with Nil
-}

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
flatMap ::
  (a -> List b)
  -> List a
  -> List b

-- flatMap f = flatten . (map f)
-- flatMap = (flatten .) . map

-- flatMap f as = foldRight (\h t -> f h ++ t) Nil as
-- flatMap f = foldRight (\h t -> f h ++ t) Nil
-- flatMap f = foldRight (\h t -> (++) (f h) t) Nil
-- flatMap f = foldRight (\h -> (++) (f h)) Nil
flatMap f = foldRight ((++) . f) Nil
-- p . q = \x -> p (q x) -- with p = (++), q = f

{-
How to think about this:
* h is the head
* t is the recursive result of the tail

This is a Monad
-}

urlEncoder :: List Char -> List Char
urlEncoder = flatMap (\c ->
  case c of
    ' ' -> '%' :. '2' :. '0' :. Nil
    '=' -> '%' :. 'e' :. 'q' :. Nil
    _ -> c :. Nil
  )

{-
When would you use flatMap?
When you have a for loop inside a for loop.
-}

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain ::
  List (List a)
  -> List a
-- flattenAgain aas = flatMap (\as -> as) aas
flattenAgain = flatMap id

{-
Notice:
flatMap f = foldRight ((++) .  f) Nil
flatten   = foldRight ((++) . id) Nil
-}

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  error "todo: Course.List#seqOptional"

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
find =
  error "todo: Course.List#find"

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
lengthGT4 =
  error "todo: Course.List#lengthGT4"

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
-- reverse Nil = Nil
-- reverse (h:.t) = reverse t ++ (h :. Nil)
-- reverse xs = foldLeft (\as a -> a :. as) Nil xs
-- reverse = foldLeft (\as a -> a :. as) Nil
reverse = foldLeft (flip (:.)) Nil

{-
(++) runs through the whole list, and it does that the list lengh times
= n * n times
= O n^2

Quadratic solution to what should be a linear solution.

-}

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f x = x :. produce f (f x)

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> \x -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse =
  error "todo: Is it even possible?"

---- End of list exercises

largeList ::
  List Int
largeList =
  listh [1..50000]

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  FilePath
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  FilePath
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Chars =
  List Char

type FilePath =
  List Char

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Functor List where
  fmap f =
    listh . P.fmap f . hlist

instance A.Applicative List where
  (<*>) =
    M.ap
  pure =
    (:. Nil)

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
