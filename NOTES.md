все есть кортеж

```smalltalk
let $ x := let $ s := 1; во первых ; - комментарий
          , print s; во вторых тут же скобки ($...;) значит новый скоуп (env)
print s ; ошибка
          
(
    def s := 1
    , print s
)
print s ; ошибка

def s_2 := $(
    print s
)
def s_1 := $(
    def s := 1
)
eval ( + s_1 s_2 ) ; 1
```
