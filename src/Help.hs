module Help where 

unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []