# fungsi (is far from complete)
---
inspired by Clojure and Scala

```smalltalk
let $ f x y := let $ r := + x y;
             , println r
             , ^r;
             
def f := @[x y | def r := + x y
                 , println r
                 , ^r]
```

it`s frozen on time while I learn Clojure and interpreters