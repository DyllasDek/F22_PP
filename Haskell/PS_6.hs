-- Danila Korneenko SD-02
type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade
data Result a 
  = Success a
  | Failure String

dup f x = f x x
dip f x = f (f x x)
twice f x = f (f x)

-- Task 1
dup :: (y -> y -> t) -> y -> t
dip :: (y -> y -> y) -> y -> y -> y
twice :: (y -> y) -> y -> y

-- Task 1.a
{-
####
dip (+) 1 2
####
dip applied to two arguments. Let us fix the type variables 
y1 for this instance (use) of dup. Then, we have the following typings:

dip :: (y1 -> y1 -> y1) -> y1 -> y1 -> y1
(+) :: Int -> Int -> Int
1 :: Int
2 :: Int

Now, we need to match the actual type of each argument of dup with the 
corresponding expected type:
  Int -> Int -> Int = y1 -> y1 -> y1 (from 1st argument)
  Int = y1 (from 2nd argument)
Then we get (+) with two arguments
  Int - > Int -> Int = y2 -> y2 -> y2 (is (+) )
  Int = y2 (from two arguments 
All of these constraints are resolved with y1 = Int and y2 = Int. Then, the type of the entire
expression then matches the type of the return value of dip, which is Int
####
-}
{-
This task racking my brain, I won't be able to do these without nerves, sorry
I hope i will understand better after getting answers in moodle
-}



-- Task 2
checkA :: Student -> Name
checkA (Student name A) = name
checkA (Student name i) = []

checkString ("") = False
checkString (other) = True

studentsHelp (x:xs)
  | null xs = [checkA(x)]
  | True = checkA(x) : studentsWithA (xs)

studentsWithA :: [Student] -> [Name]
studentsWithA ([]) = []
studentsWithA(input) = filter (\a -> checkString(a)) (studentsHelp(input))


-- Task 3

-- 3.A
checksuc (Success a) = True
checksuc (Failure a) = False
getres (Success a) = a


whileSuccess :: (a -> Result a) -> a -> a

whileSuccess func inp
  | nextCheck = whileSuccess func next
  | True = inp
  where next = getres(func inp)
        nextCheck = checksuc(func next)
{-
f n | n > 100 = Failure "input is too large"
    | otherwise = Success (2 * n)
example1 = whileSuccess f 1 -- 64
-}


-- 3.B

applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success p) (Failure b) = Failure b
applyResult (Failure p) (b) = Failure p
applyResult (Success p) (Success b) = Success (p b) 
{-
Kostyli
ex = getres2(applyResult (Failure "no function") (Failure "no arg"))
getres2 (Success a) = a
getres2 (Failure b) = "Failure " ++ b
-}
-- 3.С

fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult func leng (Success x) = func x
fromResult func leng (Failure x) = leng x

-- 3.D

combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith func (Success p) (Success b) = Success (func x y)
combineResultsWith func (Failure p) (b) = Failure p
combineResultsWith func (p) (Failure b) = Failure b
