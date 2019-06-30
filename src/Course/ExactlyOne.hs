{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

data ExactlyOne a = ExactlyOne a
  deriving (Eq, Show)

runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

-- addExactlyOne :: ExactlyOne Int -> ExactlyOne Int -> Int
-- addExactlyOne a b = (runExactlyOne a) + (runExactlyOne b)

addExactlyOne :: ExactlyOne Int -> ExactlyOne Int -> ExactlyOne Int
addExactlyOne a b =
  let
    a' = runExactlyOne a
    b' = runExactlyOne b
    result = a' + b'
  in ExactlyOne result

{-
"map" is a very important pattern - remember this pattern

Suppose we call ExactlyOne `f`

map :: (a -> b) -> f a -> f b
-}

mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a)    = ExactlyOne (f a)

{-
"bind" is a very important pattern - remember this pattern

Suppose we call ExactlyOne `f`

bind :: (a -> f b) -> f a -> f b
-}

bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

{-
Question:
Since we just need to return an ExactlyOne anyway,
what's the point of bind where the function needs
to create the ExactlyOne?
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

