module Help where

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) (drop start list)

isValidParnts :: String -> (Bool, Int, Int)
isValidParnts string = help string 0 0 0 0
    where
        help :: String -> Int -> Int -> Int -> Int -> (Bool, Int, Int)
        help [] push pop line n = (push == pop, line, n)
        help ('\n':xs) push pop line n = help xs push (pop + 1) (line + 1) (n + 1)

        help (x:xs) push pop line n | x == '(' || x == '[' =
             help xs (push + 1) pop line (n + 1)

        help (x:xs) push pop line n | x == ')' || x == ']' =
             help xs push (pop + 1) line (n + 1)

        help (x:xs) push pop line n =
             help xs push pop line (n + 1)