-- Danila Korneenko SD-02
import CodeWorld
import Data.Maybe ( mapMaybe )

-- 1.1 Lines
data Line a = Line [a] a [a]
  deriving (Show)

integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- Exercise 1.1
cutLine :: Int -> Line a -> Line a
cutLine n (Line left x right) = Line (take n left) x (take n right)


-- Exercise 1.2
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generate f (f x) []) x (generate g (g x) [])
-- Help function to generate part of line with func
    where
        generate :: (a -> Maybe a) -> Maybe a -> [a] -> [a]
        generate f (Just val) curr = generate f (f val) (val:curr)
        generate _ Nothing curr = reverse curr


-- Exercise 1.3
mapLine :: (a -> b) -> Line a -> Line b
-- Apply func to all line
mapLine func (Line left x right) 
  = Line (map func left) (func x) (map func right)


-- Exercise 1.4
-- Zip two lines together
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line l1 x1 r1) (Line l2 x2 r2)
  = Line (zip l1 l2) (x1, x2) (zip r1 r2)
-- Apply func to zipped elements 
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith func (Line l1 x1 r1) (Line l2 x2 r2)
    = Line (zipWith func l1 l2) (func x1 x2) (zipWith func r1 r2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- 1.2 Rule 30
data Cell = Alive | Dead
  deriving (Show)

-- Exercise 1.5
-- Calculate next state according to Rule 30
newState :: Cell -> Cell -> Cell -> Cell
newState Dead  Dead  Dead  = Dead   -- 0 0 0 -> 0
newState Dead  _     _     = Alive  -- 0 0 1 / 0 1 0 / 0 1 1 -> 1
newState Alive Dead  Dead  = Alive  -- 1 0 0 -> 1
newState Alive _     _     = Dead   -- 1 0 1 / 1 1 0 / 1 1 1 -> 0

-- Apply Rule 30 for specific line
rule30 :: Line Cell -> Cell
rule30 (Line [] x []) = newState Dead x Dead
rule30 (Line [] x (r:_)) = newState Dead x r 
rule30 (Line (l:_) x []) = newState l x Dead 
rule30 (Line (l:_) x (r:_)) = newState l x r 


-- Exercise 1.6
-- Shift focus to the left
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line (x:l) y r) = Just (Line l x (y:r))
shiftLeft (Line [] _ _) = Nothing
-- Shift focus to the right
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line l y (z:r)) = Just (Line (y:l) z r)
shiftRight (Line _ _ []) = Nothing

-- Create line of lines with every shift
-- Exercise 1.7
lineShifts :: Line a -> Line (Line a)
lineShifts line = Line left line right
    where
        left =  reverse(check shiftLeft (shiftLeft line) [])
        right = reverse(check shiftRight (shiftRight line) [])

        check :: (Line a -> Maybe (Line a))-> Maybe (Line a) 
          -> [Line a] -> [Line a]
        check f (Just x) out = check f (f x) (x:out)
        check _ Nothing out = out
-- Just apply rule to the every line of cells
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


-- Exercise 1.8
-- Render line
renderLine :: Line Picture -> Picture
renderLine (Line l x r) 
  = renderLeft <> x <> renderRight
  where
    -- Render left part of the picture
    renderLeft = translated (-1) 0 (rest (-1) l)
    -- Render right part of the picture
    renderRight = translated 1 0 (rest 1 r)
    -- Help func to render every part of picture
    rest :: Double -> [Picture] -> Picture
    rest d (x:xs) 
      = x <> (translated d 0 (rest d xs))
    rest _ [] = (colored white (solidRectangle 1 1))
    
-- Func that converts state to specific color
lToPicture :: Line Cell -> Line Picture
lToPicture l = mapLine (\arg -> toPicture arg) l
  where
    toPicture :: Cell -> Picture
    toPicture Alive = (colored black (solidRectangle 1 1))
    toPicture Dead = (colored white (solidRectangle 1 1))
-- Render rule 30
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n l
  | n > 1 = (renderLine (lToPicture l)) 
    <> (translated 0 (-1) (renderRule30 (n-1) (applyRule30 l)))
  | otherwise = renderLine (lToPicture l)



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- 1.3 Discrete spaces

data Space a = Space (Line (Line a)) deriving (Show)

-- Exercise 1.9
productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line l1 x1 r1) line2 
  = Space (Line lPart focus rPart)
  where
    -- Build left  part of space
    lPart = reverse(vertBuild l1 line2 [])
    -- Build focus part of space
    focus = lineBuilder x1 line2
    -- Build right part of space
    rPart = reverse(vertBuild r1 line2 [])
    
    --"multiply" line 1 variable with all line2 variables
    horizontCells :: a -> [b] -> [(a, b)] -> [(a, b)]
    horizontCells f (x:xs) out = horizontCells f xs ((f, x):out)
    horizontCells _ [] out = reverse out
    
    -- Build one horizontal line with help of "horizontCells"
    lineBuilder :: a -> Line b -> Line (a, b)
    lineBuilder f1 (Line x2 f2 z2)
      = Line (horizontCells f1 x2 []) 
             (f1, f2) 
             (horizontCells f1 z2 [])
             
    -- Build matrix with lineBuilder, save all horizontals in vertical array 
    vertBuild :: [a] -> Line b -> [Line (a, b)] -> [Line (a, b)]
    vertBuild [] _ out = out
    vertBuild (x:xs) l2 out 
      = vertBuild xs l2 (lineBuilder x l2 : out)

-- Exercise 1.10
-- Apply func to all space elements
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace func (Space (Line l x r)) = Space (Line lPart f rPart)
  where
    lPart = map (mapLine func) l
    f = mapLine func x
    rPart = map (mapLine func) r
-- Zip elements of space together in one space
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line l1 x1 r1)) (Space (Line l2 x2 r2)) 
  = Space (Line lPart f rPart)
  where
    lPart = reverse(zipRows l1 l2 [])
    f = zipLines x1 x2
    rPart = reverse(zipRows r1 r2 [])

    zipRows :: [Line a] -> [Line b] -> [Line (a, b)] -> [Line (a, b)]
    zipRows (l1:ls1) (l2:ls2) out 
      = zipRows ls1 ls2 (zipLines l1 l2 : out)
    zipRows [] _ out = out
    zipRows _ [] out = out
-- Apply func to zipped space
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith func 
  (Space(Line l1 x1 r1)) 
  (Space(Line l2 x2 r2)) 
  = Space(Line lPart f rPart)
    where
      lPart = reverse (zRow func l1 l2 [])
      f = zipLinesWith func x1 x2
      rPart = reverse (zRow func r1 r2 [])
      
      zRow :: (a -> b -> c) -> [Line a] -> [Line b] -> [Line c] -> [Line c]
      zRow func (l1:ls1) (l2:ls2) out
        = zRow func ls1 ls2 (zipLinesWith func l1 l2 : out)
      zRow _ [] _ out = out
      zRow _ _ [] out = out


--Exercise 1.11
-- Well, it's just the same as 1.10 :) 



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- 1.4 Conway’s Game of Life

--Exercise 1.12
conwayRule :: Space Cell -> Cell 
conwayRule (Space (Line top f@(Line _ cell _ ) bot)) 
  = nextG cell (neigAlive (neig top f bot) 0)
  where
    nextG :: Cell -> Int -> Cell
    nextG Dead 3 = Alive
    nextG Dead _ = Dead
    nextG Alive 3 = Alive
    nextG Alive 2 = Alive
    nextG Alive _ = Dead
    
    neigAlive :: [Cell] -> Int -> Int
    neigAlive [] n = n
    neigAlive (Alive:xs) n = neigAlive xs (n+1)
    neigAlive (Dead:xs) n = neigAlive xs n
    
    deadCells = [Dead,Dead,Dead]
    
    neigFocus :: Line Cell -> [Cell]
    neigFocus (Line [] _ []) = [Dead,Dead]
    neigFocus (Line [] _ (r:_)) = [Dead,r]
    neigFocus (Line (l:_) _ []) = [l,Dead]
    neigFocus (Line (l:_) _ (r:_)) = [l,r]
    
    neigLine :: Line Cell -> [Cell]
    neigLine (Line [] f []) = [Dead,f,Dead]
    neigLine (Line [] f (r:_)) = [Dead,f,r]
    neigLine (Line (l:_) f []) = [l,f,Dead]
    neigLine (Line (l:_) f (r:_)) = [l,f,r]
    
    
    
    neig ::[Line Cell] -> Line Cell -> [Line Cell] -> [Cell]
    neig [] f [] = deadCells ++ neigFocus f ++ deadCells
    neig [] f (r:_) = deadCells ++ neigFocus f ++ neigLine r
    neig (l:_) f [] = neigLine l ++ neigFocus f ++ deadCells
    neig (l:_) f (r:_) = neigLine l ++ neigFocus f ++ neigLine r

--Exercise 1.13
spaceShifts :: Space a -> Space (Space a)

spaceShifts space = Space (Line left (vertSShift space) right)
  where
    left = reverse ( shifts shSLeft (shSLeft space) [])
    right = reverse ( shifts shSRight (shSRight space) [])

    shifts :: (Space a -> Maybe (Space a)) -> Maybe (Space a) 
      -> [Line (Space a)] -> [Line (Space a)]
    -- Apply shifts until left
    shifts func (Just next) out = shifts func (func next) (vertSShift next:out)
    shifts _ Nothing out = out
    
    -- Apply left shift
    shSLeft :: Space a -> Maybe (Space a)
    shSLeft = shSHor shiftLeft
    
    -- Apply right shift
    shSRight :: Space a -> Maybe (Space a)
    shSRight = shSHor shiftRight
    
    -- Func for vertical shift
    vertSShift :: Space a -> Line (Space a)
    vertSShift space = Line up space down
      where
        up = reverse $ shifts shUp (shUp space) []
        down = reverse $ shifts shDn (shDn space) []

        shifts :: (Space a -> Maybe (Space a)) -> Maybe (Space a) -> [Space a] 
          -> [Space a]
        shifts _ Nothing out = out
        shifts fn (Just sp) out = shifts fn (fn sp) (sp:out)

        shUp :: Space a -> Maybe (Space a)
        shUp (Space (Line (up:top) r b))
          = Just (Space (Line top up (r:b)))
        shUp (Space (Line [] _ _)) = Nothing

        shDn :: Space a -> Maybe (Space a)
        shDn (Space (Line top r (dn:bot)))
          = Just (Space (Line (r:top) dn bot))
        shDn (Space (Line _ _ [])) = Nothing
     
    -- Func for horizontal shift
    shSHor :: (Line a -> Maybe (Line a)) -> Space a 
      -> Maybe (Space a)
    shSHor dir (Space (Line top r bot)) 
      = shSp shTop shF shBot
      where
        shTop = mapMaybe dir top
        shF = dir r
        shBot = mapMaybe dir bot

        shSp :: [Line a] -> Maybe (Line a) -> [Line a] -> Maybe (Space a)
        shSp _ Nothing _ = Nothing
        shSp x (Just y) z = Just (Space (Line x y z))

applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)


-- Exercise 1.14
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line top r bot)) 
  = topR <> fR <> botR
  where    
    topR = translated 0 (-1) (rest (-1) top)
    fR = renderLine r
    botR = translated 0 1 (rest 1 bot)

    rest sh (x:xs) 
      = (renderLine x) <> (translated 0 sh (rest sh xs))
    rest _ [] = (colored white (solidRectangle 1 1))

fromStoPic :: Space Cell -> Space Picture
fromStoPic sp = mapSpace toPic sp
  where
    toPic :: Cell -> Picture
    toPic Alive = (colored black (solidRectangle 1 1))
    toPic Dead = (colored white (solidRectangle 1 1))

conwayStep :: Int -> Space Cell -> Space Cell
conwayStep 0 sp = sp
conwayStep n sp = conwayStep (n-1) (applyConwayRule sp)
  
  

animateConway :: Space Cell -> IO ()
animateConway sp = animationOf renderNextStep
  where
    renderNextStep :: Double -> Picture
    renderNextStep sec 
      = renderSpace (fromStoPic (conwayStep (floor sec) sp))

-- Exercise 1.2 help func
gen21:: Double-> Maybe Double
gen21 0.0 = Nothing
gen21 x = Just (x - 2)

gen22:: Double-> Maybe Double
gen22 8.0 = Nothing
gen22 x = Just (x + 2)

-- Exercise 1.6 help func
pars :: Maybe (Line Integer) -> Line Integer
pars (Just x) = x
pars Nothing = Line [] 0 []

-- Exercise 1.12 help var
pulsator :: Space Cell
pulsator = Space(
    Line 
        [
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead))
        ]
        (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
        [
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead))
        ])
glider :: Space Cell
glider = Space(
    Line 
        [
            Line [Alive, Dead, Dead, Dead, Dead] Dead [Alive, Dead, Dead, Dead, Dead],
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead))
        ]
        (   Line ((replicate 5 Dead)) Alive [Alive, Dead, Dead, Dead, Dead])
        [
            Line ((replicate 5 Dead)) Alive ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead)),
            Line ((replicate 5 Dead)) Dead ((replicate 5 Dead))
        ])
main:: IO()
main = do 
  putStrLn "Ex1:"
  print (cutLine 3 integers)
  putStrLn ""
  putStrLn "Ex2:"
  print (genLine gen21 4 gen22)
  putStrLn ""
  putStrLn "Ex3:"
  -- Infinite list, so uncomment with caution
  -- print (mapLine (^2) integers)
  print (cutLine 3 (mapLine (^2) integers))
  putStrLn ""
  putStrLn "Ex4 simple zip:"
  print (cutLine 2 (zipLines integers integers))
  putStrLn "Ex4 zip with:"
  print (cutLine 3 (zipLinesWith (*) integers integers))
  putStrLn ""
  putStrLn "Ex5 with 0 1 1 :"
  print (rule30 (Line [Dead] Alive [Alive]))
  putStrLn ""
  putStrLn "Ex6 left shift:"
  print(cutLine 3 (pars (shiftLeft integers)))
  putStrLn "Ex6 right shift:"
  print(cutLine 3 (pars (shiftRight integers)))
  putStrLn ""
  putStrLn "Ex7 line shift:"
  print(lineShifts (Line [-1, -2, -3] 0 [1, 2, 3]))
  putStrLn ""
  putStrLn "Ex7 apply rule 30:"
  print(applyRule30(Line [Dead] Alive [Dead]))
  putStrLn ""
  -- Ex 8
  -- drawingOf(renderRule30 11 (Line ((replicate 11 Dead)) Alive ((replicate 11 Dead))))
  
  putStrLn "Ex9:"
  print (productOfLines (cutLine 3 integers) (cutLine 3 integers))
  putStrLn ""
  putStrLn "Ex10 mapSpace:"
  print (mapSpace (\x -> fst x + snd x) (productOfLines (cutLine 3 integers) 
           (cutLine 3 integers)))
  putStrLn ""
  putStrLn "Ex10 zipSpace:"
  print (zipSpaces (productOfLines (cutLine 3 integers) (cutLine 3 integers)) 
          (productOfLines (cutLine 3 integers) (cutLine 3 integers)))
  putStrLn ""
  putStrLn "Ex10 zipSpaceWith:"
  print (zipSpacesWith (\x y -> (x, fst y)) (productOfLines (cutLine 3 integers) 
    (cutLine 3 integers)) (productOfLines (cutLine 3 integers) (cutLine 3 integers)))
  putStrLn ""
  putStrLn "Ex12:"
  print (conwayRule pulsator)
  putStrLn ""
  putStrLn "Ex13:"
  print (applyConwayRule pulsator)
  
  -- pulsator pattern
  animateConway pulsator
  
  -- glider pattern
  -- animateConway glider