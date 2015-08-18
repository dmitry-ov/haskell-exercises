type Komp = (Int, Int)
(+%) :: Komp -> Komp -> Komp
(r1, i1) +% (r2, i2)    = (r1+r2, i1+i2)

(*%) :: Komp -> Komp -> Komp
(r1, i1) *% (r2, i2)    = (r1*r2 - i1*i2, r1*i2 + i1*r2)

(^%) :: Komp -> Int -> Komp
k ^% 1      = k
k ^% n      = (k ^% (n-1)) *% k

vredKompPol :: [(Komp,Int)] -> Komp -> Komp
vredKompPol ((k,s):poli) t  = k*%(t^%s) +% (vredKompPol poli t)