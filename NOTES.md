```smalltalk
def true := object
true <- @[f s | ^f]
true.str <- "true"
println (if true then true else false)

def obj := [o1]
def obj2 := object
def f := object <- @[ap | ... ].check [o1] 
obj2.f <-  @[ap | ... ].check obj
def a := o1
def b := object
f a -- ok
obj2.f a -- ok
f b -- error
obj2.f b -- error
```
    