* Problem 3
* Largest prime factor

* The prime factors of 13195 are 5, 7, 13 and 29.
* What is the largest prime factor of the number 600851475143?

> import Data.Sequence as S
> import Prelude as P
> import Data.Maybe

> isFactorOf :: Integral a => a -> (a -> Bool)
> isFactorOf x =  (0 ==) . (x `rem`)

> primesUpto :: Integral a => a -> Seq a
> primesUpto limit =
>     sieve $ fromList [2..limit]
>       where
>         sieve remainingPossibilities
>             | finished  = remainingPossibilities
>             | otherwise = prime <| (sieve . S.filter ((0 /=) . (`rem` prime)) $ possiblySeq)
>           where
>             prime :< possiblySeq = viewl remainingPossibilities
>             finished             = (prime ^ 2) > limit

One method to obtain the largest prime factor is by producing a sequence of prime numbers upto (n / 2),
    and then checking if they are a factor of n -- in descending order to terminate early.

> largestPrimeFactorOf :: Integral a => a -> a
> largestPrimeFactorOf n  = largestFactor
>   where
>     descendingPossibilities = S.reverse . primesUpto $ n `div` 2
>     largestFactor :< _      = viewl . S.filter (isFactorOf n) $ descendingPossibilities

However, [primesUpto n] is ~O(n^1.5) and a cheaper solution is possible.
Generating the full sequence of potential prime factors is unnecessary.

> largestPrimeFactorOf' :: Integral a => a -> a
> largestPrimeFactorOf' x = case smallestFactor x of
>                             Nothing -> x
>                             Just f  -> largestPrimeFactorOf' $ max f (x `div` f)
>   where
>     smallestFactor :: Integral a => a -> Maybe a
>     smallestFactor m = listToMaybe . P.filter (isFactorOf m) $ [2..(ceiling . sqrt . fromIntegral $ m)]

> result :: Int
> result = largestPrimeFactorOf' 600851475143

> main :: IO ()
> main = print result
