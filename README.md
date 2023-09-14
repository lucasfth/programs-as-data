# programs-as-data

## Created by

fefa and luha

## Assignment 1

Only changed Intcomp1.fs and Intro2.fs.
Also created the file SimpleExpr.java

## Assignment 2

### Exercise 3.2

The regex for matching on a string containing `a` and `b` where there had to be an `b` between all `a`'s, could be:

```regex
^a?((b+)(a?))*$
```

NFA diagram
```mermaid
flowchart TD

ids((start))
id1((1))
id2((2))
id3((3))
ide(((end)))

ids -- ε --> id1
id1 -- a --> id2
id1 -- b --> id3
id2 -- ε --> ide
id2 -- b --> id3
id3 -- ε --> ide
id3 -- ε --> id1
```

DFA diagram
```mermaid
flowchart TD

ids((start))
id1(((1)))
id2(((2)))
ids -- a --> id1
ids -- b --> id2
id1 -- b --> id2
id2 -- a --> id1
id2 -- b --> id2
```
