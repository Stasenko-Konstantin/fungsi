module Help where 

unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) (drop start list)