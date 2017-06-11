# Chapter 13 Monadic parsing

## `1^(2+3)^4` trees:

```
                   expr
                    |
                   term
                    |
                  factor
                    |
                    ^
                    |
     ---------------------
    /                     \
primitive               factor
    |                     |
    1                     ^
                          |
                     ----------------
                    /                \
                primitive          factor
                    |                |
                   expr          primitive
                    |                |
                    +                4
                    |
                ----------
               /          \
              expr       term
               |          |
              term      factor
               |          |
             factor   primitive
               |          |
           primitive      3
               |
               2
```
```
    ^
    |
 -----
/     \
1     ^
      |
     ----
    /    \
    +    4
    |
    --
   /  \
   2  3
```


## `1-2^3*4` trees:

```
                   expr
                    |
                    -
                    |
     ---------------------
    /                     \
   expr                  term
    |                     |
   term                   *
    |                     |
  factor             ----------------
    |               /                \
primitive          term            factor
    |               |                |
    1             factor         primitive
                    |                |
                    ^                4
                    |
                ----------
               /          \
           primitive    factor
               |          |
               2      primitive
                          |
                          3
```
```
    -
    |
 -----
/     \
1     *
      |
     ----
    /    \
    ^    4
    |
    --
   /  \
   2  3
```