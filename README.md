# fungsi (is far from complete)
---
#### ! Its just my sandbox !                               
Inspired by Lisps. And many other languages but mainly on Lisps

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

It`s frozen on time while I learn Clojure and interpreters
