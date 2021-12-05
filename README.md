# fungsi (еще далек от завершения)
Небольшой функциональный язык программирования для простых математических расчетов


### Примеры
< - приглашение REPL\`а                                                                 
\> - вывод интерпретатора                                                                                             
| - продолжение ввода

```haskell
< 1 + 1 
> 2

< a := 5!
> a = 25

< A := 26 -- язык регистронезависим, а все "переменные", на самом деле константы
> Override error: a = 25

< if a /= 5! then true else false -- ветка else обязательная
> false

< 5 = 5.0
> true

< 5 == 5.0
> false

< f := a b c -> if a = b then sqrt(c) else с^a -- лямбда
> f = Func => (Num, Num, Num) -> Num

< f 1 2 3
> 3

< b := (1 2 3) -- кортеж
> b = (Int, Int, Int)

< b = [1 2 3] -- квадратные скобки = круглым, они нужны для лучшей читаемости
> true

< f b
> 3

< f2 := ->
> f2 = Func => Nil

< f2
> nil

< log 0 - 1
> nil

< f3 := 1 +
| 1
> f3 = 2

< type log
> Func => Num | Nil

< f4 := a ->
| sqrt (log a)
> Raw Nil error: log => Num | Nil; sqrt ( param => Num )

< f4 := a ->
| if isNil (log a) then nil else sqrt (log a)
> f4 = Func => Num | Nil

< quit
$
```
