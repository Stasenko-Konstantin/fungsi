# fungsi
inspired by Scala and Racket

```smalltalk
let $ f x y := let $ r := + x y;
             , println r
             , ^r;
             
def f := @[x y | def r := + x y
                 , println r
                 , ^r]
```

