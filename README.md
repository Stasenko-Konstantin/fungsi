# fungsi (is far from complete)
A small functional programming language for mathematical calculations


### Examples
< - REPL prompt                                                                 
\> - interpreter output

```haskell
< 1 + 1 
> 2

< a := 5!
> a = 25

< A := 26 -- language is case-independent, and all "variables" are actually constants
> Override error: a = 25

< if a /= 5! then true else false -- branch else mandatory
> false

< 5 = 5.0
> true

< 5 == 5.0
> false

< f := a b c -> if a = b then sqrt(c) else с^a -- lambda
> f = Func => (Num, Num, Num) -> Num

< f 1 2 3
> 3

< b := (1 2 3) -- кортеж
> b = (Int, Int, Int)

< b = [1 2 3] -- square brackets are equal to round brackets, they are necessary for better readability
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
