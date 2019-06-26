# Notes

## More study

https://www.seas.upenn.edu/~cis194/fall16/

## Syntax

To start `ghci` without loading the `.ghci` file.

```
ghci -ignore-dot-ghci
```

## Patterns

| Pattern                             | Notes                            |
| ----------------------------------- | -------------------------------- |
| \x -> f x                           | f                                |
| \x -> f (g x)                       | f . g                            |
| (a -> b) -> (t -> a) -> (t -> a)    | .                                |
| foldRight (\h t -> f h ++ t) Nil as | h is head, t is recursive result |

## Compare

| English   | Haskell    |
| --------- | ---------- |
| and then  | >>=        |
| always    | pure       |
| or        | `|||`      |
| 0 or many | list       |
| 1 or many | list1      |
| exactly n | thisMany n |
| call it x | \x ->      |

## Quiz

Q: In function type declarations, is `->` right or left associative?
A: Right assiciative

Q: What is the `cons` operator?
A: `(:.)`

Q: Does `foldRight` or `foldLeft` do constructor replacement?
A: `foldRight`

Q: Does `foldRight` or `foldLeft` implement a linear loop?
A: `foldLeft`

Q: What is the type for `const`?
A: `const :: a -> b -> a`

Q: What is the type for `.`?
A: `(b -> c) -> (a -> b) -> a -> c`

Q: What can `\x -> f x` the following be reduced to?
A: `f`

Q: What can `\x -> f (g x)` be recuded to?
A: `f . g`
