{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

printHello :: IO ()
printHello = putStrLn "Hello"

printHelloWorld :: IO ()
printHelloWorld = do
  putStrLn "Hello"
  putStrLn "World"

printElements :: [Chars] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

putLines1 :: Chars -> Chars -> IO()
putLines1 s1 s2 = do
  putStrLn s1
  putStrLn s2

putLines2 :: Chars -> Chars -> IO()
putLines2 s1 s2 =
  putStrLn s1 >>= (\_ -> putStrLn s2)


-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()

-- printFile path content = do
--   putStrLn ("============ " ++ path)
--   putStrLn (content)

{-
See note about do notation:
https://wiki.haskell.org/Do_notation_considered_harmful
-}

-- printFile fp c =
--   putStrLn ("============ " ++ fp) >>= \_ ->
--     putStrLn c

printFile fp c =
  let putLn1 = putStrLn ("============ " ++ fp)
      putLn2 = putStrLn c
  in  putLn1 >>= (\_ -> putLn2)
  

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()

-- printFiles xs =
--   let x = (\t -> printFile (fst t) (snd t)) <$> xs    -- List (IO ())
--       y = sequence x                                  -- IO (List ())
--       z = void y                                      -- IO ()
--   in z

printFiles xs =
  let x = (uncurry printFile) <$> xs    -- List (IO ())
      y = sequence x                    -- IO (List ())
      z = void y                        -- IO ()
  in z

{-
Note about uncurry

uncurry takes a function of type

  a -> b -> c

and changes it to:

  (a, b) -> c

I.e. accepting a tuple.

Consider the type of printFile:

  printFile :: FilePath -> Chars -> IO ()

When we apply uncurry, we get:

  (FilePath, Chars) -> IO ()

And this (FilePath, Chars) is nicer to work with here

Note that ideally this would be written as:

  printFiles =
    traverse_ (uncurry printFile)

But we haven't covered that material yet


-}

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)

-- getFile fp =
--   let f = readFile fp
--   in (\content -> (fp, content)) <$> f

getFile fp =
  (\content -> (fp, content)) <$> readFile fp

{-
The hint suggests we use readFile

readFile is provided by Haskell

Type:

  readFile :: FilePath -> IO Chars

So if we read the file, we have

  IO Chars

But we want

  IO (filePath, Chars)

So we need a function:

  IO Chars -> IO (filePath, Chars)

Which pattern does this match?

  f a -> f b

Where f is IO.

Looks like we can use fmap <$> again

  (a -> b) -> f a -> f b


-}

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))

-- getFiles fps =
--   let ios = getFile <$> fps
--   let s = sequence ios
--   in s

getFiles =
  sequence . (getFile <$>)
  
{-
The hint here suggests we use getFile.

Looking at types, we have:

  getFiles :: List FilePath -> IO (List (FilePath, Chars))
  getFile :: FilePath -> IO (FilePath, Chars)

Something to recall here is this pattern:

  sequence :: List (f a) -> f (List a)

In our case:
  f is IO and
  a is (FilePath, Chars)

Which gives us:

    List (IO (FilePath, Chars)) -> IO (List (FilePath, Chars))

So if we can achieve `List (IO (FilePath, Chars))`
then we can get the result using `sequence`.

In that case, we can try to achieve this:

  List FilePath -> List (IO (FilePath, Chars))

This is a simple example of f a -> f b

And we need a function that has this type:

  FilePath -> IO (FilePath, Chars)

Which is an a -> b, and matches our `getFile` type.

So we need a function with this pattern:

  (a -> b) -> f a -> f b

Which is `fmap`, or `<$>`

-}

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()

-- run fp =
--   readFile fp >>= \content ->
--     getFiles (lines content) >>= printFiles

run fp = do
  content <- readFile fp
  files <- getFiles (lines content)
  printFiles files

{-
do notation is syntax sugar for calling bind multiple times

do removes the nesting

purely syntactic, to be more readable
-}
 
-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()

-- main =
--   getArgs >>= (\xxs ->
--     case xxs of
--       Nil ->
--         putStrLn "No arguments"
--       x :. _ -> 
--         run x
--   )

main = do
  xxs <- getArgs
  case xxs of
    Nil ->
      putStrLn "No arguments"
    x :. _ -> 
      run x
 
----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
