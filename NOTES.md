```smalltalk
true := object
true <- @[f s | ^f]
true.str <- :true
println (if true then true else false)

obj := [o1]
obj2 := object
f := object <- @[ap | ... ].check [o1] 
obj2.f <-  @[ap | ... ].check obj
a := o1
b := object
f a -- ok
obj2.f a -- ok
f b -- error
obj2.f b -- error

ones := object
ones <- @[| 1 .: ones]
println (take 3 ones) NB [1,1,1]
```
    