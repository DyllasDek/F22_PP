main :: IO()
-- Task 1.a
binaryToDecimal :: [Int] -> Int
func :: ([Int]) -> Int
binaryToDecimal input = func (reverse input)
func [] = 0
func (x:xs) = x + 2 * func xs

-- Task 1.b
countZeros :: ([Int]) -> Int
countHelp :: ([Int],Bool) -> Int

countZeros input = countHelp (input,False)
-- var is if it exclude counting zeros
countHelp (x:xs, var)
  | null xs = -1*(x-1)
  | (x == 1) = 0 + countHelp (xs , True)
  | (var && x == 0) =  1 + countHelp(xs , True)
  | True = 0 + countHelp(xs, var)


-- Task 1.c
encodeWithLengths :: [Int] -> [Int]
encodeHelp :: ([Int],Int,Int,[Int]) -> [Int]
encodeEnd :: (Int,Int,Int,[Int]) -> [Int]
encodeEnd (x,elem,counter,out)
  | x == elem = out ++ [(counter+1)]
  | True = out ++ [(counter)]

removeZeros ((x:xs))
  | x /= 1 = removeZeros(xs)
  | True = (x:xs)
  
first ((x:xs)) = x

encodeHelp (x:xs,elem,counter,out)
  | null xs = encodeEnd(x,elem,counter,out)
  | (x == elem) = encodeHelp (xs, elem,counter+1,out)
  | (x /= elem) = encodeHelp ((x:xs),x,0,out ++ [counter])

encodeWithLengths input = encodeHelp (removeZeros(input),first(removeZeros(input)),0,[])


-- Task 1.d
binaryOdd :: [Int] -> Bool
binaryOdd (x:xs)
  | null xs = x==1
  | otherwise = binaryOdd xs
binaryOdd [] = False  --for empty list conditions


-- Task 1.e

decrement :: [Int] -> [Int]
decrementHelp :: [Int] -> [Int]
decrement input = removeZeros (reverse (decrementHelp (reverse input)))

decrementHelp (x:xs)
  | x == 1 = 0 : xs
  | null xs = [0]
  | True = 1 : decrementHelp(xs)


main = print(decrement [1,0,1,1,0])


-- Task 1.f
propagate :: (Bool,[Int]) -> [(Bool,Int)]

propagate (val,inp) = propagateHelp(val,inp,[])

propagateHelp (val,(x:xs),out)
  | null xs = out ++ [(val,x)]
  | True = propagateHelp(val,xs,out ++ [(val,x)])



-- Task 2.a

alternatingSum :: [Int] -> Int

altHelp:: ([Int],Int,Int) ->Int

alternatingSum input = altHelp (input,1,0)

altHelp ((x:xs), i, out)
  | null xs = (out + i * x)
  | True = altHelp (xs,(-1*i),(out + i * x))
  
-- Task 2.b
-- alternating-sum [1, 2, 3, 4, 5] 
-- altHelp [1, 2, 3, 4, 5] 1          0
-- altHelp [2, 3, 4, 5]  (-1 * 1)    (0 + 1 * 1)
-- altHelp [2, 3, 4, 5]   -1          1
-- altHelp [3, 4, 5]     (-1 * -1)   (1 + -1 * 2)
-- altHelp [3, 4, 5]       1         -1
-- altHelp [4, 5]        (-1 * 1)   (-1 + 1 * 3)
-- altHelp [4, 5]         -1          2)
-- altHelp [5]           (-1 * -1)  (2 + -1 * 4)
-- altHelp [5]             1          -2)
-- altHelp [] => (-2 + 1*5) => 3


-- Task 3
data Radians = Radians Double
data Degrees = Degrees Double
pin :: Double
pin = 3.14159

toDegrees :: Radians -> Degrees
fromDegrees :: Degrees -> Radians

toDegrees (Radians rad) = Degrees (rad * 180.0 / pin)
fromDegrees (Degrees deg) = Radians (deg * pin / 180.0)

