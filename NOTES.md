```smalltalk
let $ x := let $ s := 1; во первых ; - комментарий
          , print s; во вторых тут же скобки ($...;) значит новый скоуп (env)
print s ; ошибка
          
(
    s := 1
    , print s
)
print s ; ошибка

s_2 := $(
    print s
)
s_1 := $(
    s := 1
)
eval ( + s_1 s_2 ) ; 1
```