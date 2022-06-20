```smalltalk
def true := object
true <- @[f s | ^f]
true <- @[| ^:true]
println (if true then true else false)
```
