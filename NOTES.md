```ats
package name // обязательная инструкция

import github.com/user/lib // квалифицированный импорт

import // табуляция необходима
    _ = lib // неквалифицированный импорт
    l = lib // псевдоним
    
export 
    Type
    func
    _ // экспорт всего пакета
    
enum T a = C1 (T2 a) | C2 a deriving Iface1 (Iface2 a)
    where 
        a <= Iface

record T a = 

interface I
```









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





---

Local state is sometimes useful for caching computed values or allowing a computation to be evaluated lazily, i.e., only once and only on demand. The procedure lazy below accepts a thunk, or zero-argument procedure, as an argument. Thunks are often used to "freeze" computations that must be delayed for some reason, which is exactly what we need to do in this situation. When passed a thunk t, lazy returns a new thunk that, when invoked, returns the value of invoking t. Once computed, the value is saved in a local variable so that the computation need not be performed again. A boolean flag is used to record whether t has been invoked and its value saved.

```scheme
(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      (lambda ()
        (if (not flag)
            (begin (set! val (t))
                   (set! flag #t)))
        val))))
```

Lazy evaluation is especially useful for values that require considerable time to compute. By delaying the evaluation, we might avoid computing the value altogether, and by saving the value, we avoid computing it more than once.

The operation of lazy can best be illustrated by printing a message from within a thunk passed to lazy.

```scheme
(define p
  (lazy (lambda ()
          (display "Ouch!")
          (newline)
          "got me")))
```

The first time p is invoked, the message Ouch! is printed and the string "got me" is returned. Thereafter, "got me" is returned but the message is not printed. The procedures display and newline are the first examples of explicit input/output we have seen; display prints the string without quotation marks, and newline prints a newline character.