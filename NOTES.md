```smalltalk
true := @[f s | ^f]
true.str <- :true
println (if { true } then { true } else { false })

obj := o1
obj2 := object
f := @[ap : o1 | ... ] 
obj2.f <- @[ap : obj | ... ]
a := o1
b := object
f a      NB ok
obj2.f a NB ok
f b      NB error
obj2.f b NB error

ones := @[| cons 1 ones]
println (take 3 ones) NB [1,1,1]
```
    