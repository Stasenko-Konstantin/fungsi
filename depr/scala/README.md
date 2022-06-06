# fungsi (is far from complete)
---
! Its just my sandbox !                               
inspired by Lisps and Scala

- call-by-value
- HOF
- macros

```smalltalk
let $ f x y := let $ r := + x y;
             , println r
             , ^r;
             
def f := @[x y | def r := + x y
                 , println r
                 , ^r]
```

it`s frozen on time while I learn Clojure and interpreters
