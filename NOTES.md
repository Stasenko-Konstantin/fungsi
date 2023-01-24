```ats
true := (f, s) -> f
// or
true (f, s) := f
true.str <- :true
println , if true then true else false // :true

obj1 := object
obj2 := obj1
obj3 := object
    f <- (ap :obj2) -> ...
// or
obj3.f <- (ap :obj2) -> ... R
f := (ap :obj1) -> ...  
a := obj1
b := object
f a      // ok
obj3.f a // ok
f b      // error
obj3.f b // error

ones := 1::ones
println , take 3 ones // [1,1,1]

factorial n := if n == 1
               then 1
               else fact , n - 1
```
    
fungsi install - либо устанавливает исполняемый файл, либо устанавливает библиотеку в локальный репозиторий (?)