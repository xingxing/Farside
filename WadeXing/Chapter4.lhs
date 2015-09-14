> module WadeXing.Chapter4 where

> import PicturesSVG
> import Chapter4 hiding( whiteBlack, whiteChess )


4.25

> whiteBlack :: Integer -> Picture
> whiteBlack n
>    | n <= 1    = white
>    | otherwise = white `beside` blackWhite (n -1)

> whiteChess :: Integer -> Integer -> Picture
> whiteChess n m
>    | n <=1     = whiteBlack m
>    | otherwise = whiteBlack m `above` blackChess (n-1) m


4.26

> column :: Picture -> Integer -> Picture
> column pic n
>    | n <= 1    = pic
>    | otherwise = pic `above` (column pic (n -1))


4.27

> row :: Integer -> Integer -> Picture
> row n b
>     | n <= 1    = point n b
>     | otherwise = (point n b) `beside` (row (n-1) b)
>     where
>     point n b = if n==b then black else white

> row2 :: Integer -> Integer -> Integer -> Picture
> row2 n b b2
>     | n <= 1    = point n b b2
>     | otherwise = (point n b b2) `beside` (row2 (n-1) b b2)
>     where
>     point n b b2 = if (n==b) || (n==b2) then black else white


$ . . .   o----> m
. $ . .   |
. . $ .   |
. . . $   v
          n

> leftToRight :: Integer -> Integer -> Picture
> leftToRight n m
>     | n <=1 = row m 1
>     | otherwise = row m n `above` (leftToRight (n-1) m)

. . . $   o----> m
. . $ .   |
. $ . .   |
$ . . .   v
          n

> rightToLeft :: Integer -> Integer-> Picture
> rightToLeft n m
>     | n <= 1 = row m m
>     | otherwise = row m (m+1-n) `above` (rightToLeft (n-1) m)

$ . . $
. $ $ .
. $ $ .
$ . . $

> x :: Integer -> Integer -> Picture
> x n m 
>     | n <=1 = row2 m 1 m
>     | otherwise = row2 m n (m+1-n) `above` (x (n-1) m)
