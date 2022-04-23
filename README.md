# fungsi (is far from complete)
inspired by Scala and Lisp

```smalltalk
let $ f x y := let $ r := + x y;
             , println r
             , ^r;
             
def f := @[x y | def r := + x y
                 , println r
                 , ^r]
```

