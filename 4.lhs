* Problem 4
* Largest palindrome product

* A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
* Find the largest palindrome made from the product of two 3-digit numbers

> import Control.Monad

> isPalindromic :: Show a => a -> Bool
> isPalindromic = liftM2 (==) id reverse . show

> result :: Int
> result = maximum [p | x <- [999, 998 .. 100], y <- [999, 998 .. x], let p = x * y, isPalindromic p]

> main :: IO ()
> main = print result
