# Cheatsheet

## Patterns

```hs
fmap :: (a -> b) -> f a -> f b

apply :: f (a -> b) -> f a -> f b

bind :: (a -> f b) -> f a -> f b
```

## Functions

| Function | Description                 |
| -------- | --------------------------- |
| id(a)    | Returns the passed in value |
