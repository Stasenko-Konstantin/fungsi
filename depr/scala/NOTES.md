```smalltalk
let $ x := let $ s := 1; во первых ; - комментарий
          , print s; во вторых тут же скобки ($...;) значит новый скоуп (env)
print s ; ошибка
          
(
    def s := 1
    , print s
)
print s ; ошибка
```
