# fungsi
inspired by Scala and Common Lisp                     
looks like ML and Lisp

```smalltalk
let f x y := $(let r := + x y,
             println r,
             ^r)
             
def f := @[x y | def r := + x y,
                 println r,
                 ^r]
```