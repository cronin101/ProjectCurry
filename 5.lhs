* Problem 5
* Smallest multiple

* 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
* What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

Observation: The smallest multiple of [1..n] must be a multiple of [1..(n-1)].
    Thus, we can step by smallest multiple of [1..(n-1)] until we get there:

> smallestMultiple1To :: Integral a => a -> a
> smallestMultiple1To 1 = 1
> smallestMultiple1To n = head . dropWhile ((/= 0) . (`rem` n)) . iterate (+ previous) $ previous
>   where previous = smallestMultiple1To (n - 1)

Alternatively, smallestMultiple([1..n]) === lcm(smallestMultiple([1..n-1]), n):

> smallestMultiple1To' :: Integral a => a -> a
> smallestMultiple1To' = foldl1 lcm . enumFromTo 1

> result :: Int
> result = smallestMultiple1To' 20

> main :: IO ()
> main = print result
