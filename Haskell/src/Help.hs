module Help where 

unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c