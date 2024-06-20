```haskell
package name // обязательная инструкция

import github.com/user/lib // квалифицированный импорт

import // табуляция необходима
  _ = lib // неквалифицированный импорт
  _ = lib ! f // импорт lib кроме f 
  _ = lib ! f1 f2 f3 
  l = lib // псевдоним
    
export 
  Type
  func
  _ // экспорт всего пакета
  _ ! f // эскопрт всего пакета кроме f
  _ ! f1 f2 f3
    
// типы всегда с большой буквы
enum T a = C1 (T2 a) | C2 a deriving Iface1 (Iface2 a)
  where 
    a <= Iface

t :T (:Int) = C2 1

record T a = C field1 :a field2 field3 :T2 // field2 и field3 принадлежат типу T2

t :T (:Int) arg1 arg2 :T2 = C 1 arg1 arg2 

interface I a where a <= Iface = 
  f1 :String :a

true f s = f
  where
    show true = "true"

show true = "true"


println <| if true then true else false // "true"

ones = 1::ones
println <| take 3 ones // [1,1,1]

factorial n = if n == 1
              then 1
              else fact <| n - 1
     
// каждый тип в сигнатуре позиционен, поэтому можно указать только некоторые типы, а остальные fungsi попытается вывести
// f :b - тип возвращаемого значения f
// arg1 arg2 :a - тип первого аргумента выводится. внешне похоже на гошное [A](arg1, arg2 A), но механизм другой
// каждый тип не в объявлении типов выделяется двоеточием. каждый тип принимающий типовые параметры принимает их в скобках через пробел
// тип :Func (:a :b :c) читается как - функция принимающая два параметра типа :a и :b и возвращающая тип :c
// отдельного синтаксиса для контекстов нет, т.е. кортежи, списки и т.п. указываются обычными типами и типовыми параметрами     
f :b arg1 arg2 :a func :Func (:Tuple2 (:a :a) :b) = func <| Tuple2 arg1 arg2

// соответствие интерфейсу указывается с помощью блока where в сигнатуре функции
// блок where есть и для тела функции, работает аналогично хаскельному where
f arg :a 
  where
    a <= Iface
  = ...
    where
      ...
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