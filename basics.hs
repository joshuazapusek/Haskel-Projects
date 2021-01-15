import Data.Char
import Prelude hiding (map)

main :: IO ()

add :: Int -> Int -> Int --dec
fact :: Int -> Int 
factGuard :: Int -> Int
roots :: (Float, Float , Float) -> (Float, Float)
factRec :: Int -> Int
map :: (a -> b) -> [a] -> [b]

fact 0 = 1
fact n = n * fact (n - 1)
factGuard n | n == 0 = 1
             | n /= 0 = n * factGuard(n-1)
add x y = x + y
roots (a,b,c) = (x1, x2) where
    x1 = e + sqrt d / (2*a)
    x2 = e - sqrt d / (2*a)
    d = b * b - 4 * a * c 
    e = -b / (2*a)
factRec 0 = 1
factRec n = n * factRec(n-1)
map _ [] = []
map func (x : abc) = func x : map func abc

main = do 
    putStrLn "Addition"
    print(add 2 5) 
    putStrLn "fact"
    print(fact 8)
    putStrLn "fact guards"
    print (factGuard 5)
    putStrLn "roots of x^2-8x+6"
    print(roots(1,-8,6))
    putStrLn "fact"
    print(factRec 5)
    putStrLn "high order"
    print(map toUpper "joshuazapusek")
    putStrLn "lambda"
    print((\x -> x+1) 4)