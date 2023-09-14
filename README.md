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

DFA diagram
```mermaid
stateDiagram-v2

[*] --> a
[*] --> b
b --> a
b --> b
a --> b
b --> [*]
a --> [*]
```
