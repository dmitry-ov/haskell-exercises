inverse :: [(a,b)] -> [(b,a)]
inverse [] = []
inverse ((x,y):xs) = (y, x):inverse xs
