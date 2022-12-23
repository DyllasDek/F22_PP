
isSingleton :: [a] -> Bool
isSingleton [arr] = True
isSingleton [] = False
isSingleton (_ : xs) = False

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert val (x:xs)
  | val <= x = val:x:xs
  | True = x : insert val xs

separateBy :: a -> [a] -> [a]
separateBy _ [] = []
separateBy _ [x] = [x]
separateBy val (x:xs)= x:val: separateBy val xs

splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot a []=([],[])
splitWhenNot f mas = help [] [] f mas
  where
    help:: [a]-> [a]-> (a-> Bool)->[a]-> ([a], [a])
    help bef aft func []  = (bef, aft)
    help bef aft func (x:xs)
      | func x = help (x:bef) aft func xs 
      | True = (reverse bef, x:xs)


groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy f x = case x of
  [] -> []
  _ -> takeWhile (not . f) x : groupsSeparatedBy f (dropWhile f (dropWhile (not . f) x))


replicateWithPos :: [a] -> [a]
replicateWithPos z = repl 1 z 
  where
      multiply len x = case len of
        0 -> []
        otherwise -> x : multiply (len - 1) x
      repl index (x:xs) = case xs of
        [] -> multiply index x
        otherwise -> multiply index x ++ repl (index + 1) xs

lucas :: [Int]
lucas = 2:1 :  zipWith (+) lucas (selftail lucas)
  where
    selftail (x:xs)=xs
    selftail []=[]

-- Task 2.b
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = approxReal 
  where
  approxReal = 1 : 1.5 : zipWith f approxReal (selftail approxReal) where 
      selftail (x:xs) = xs
      f a b = b - (b / 2) + (1 / b)




main :: IO ()
main = do
  print $ isSingleton [1]
  print $ isSingleton [1 ..]
  print $ isSingleton [[1 ..]]

  print $ separateBy ',' "hello"
  print $ take 5 (separateBy 0 [1..])
  print $ take 10 (snd (splitWhenNot (< 100) [1..]))
  print $ take 5 (separateBy 0 [1..])
  print $ take 10 lucas



