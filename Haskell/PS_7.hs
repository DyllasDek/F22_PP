import Data.Char (toUpper)

echo :: IO ()
echo = do
  putStrLn "Input your string: "
  input <- getLine
  case input of
    "EXIT" -> putStrLn "Goodbye!"
    inp -> do
      putStrLn (map toUpper inp)
      echo

foreverIO :: IO a -> IO b
foreverIO action = do
  action
  foreverIO action

whenIO :: Bool -> IO () -> IO ()
whenIO flag func = do
  case flag of
    True -> func
    False -> return ()

maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO action = do
  case action of
    Nothing -> return (Nothing)
    Just action -> do
      ans <- action
      return (Just ans)

sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (func : functions) = do
  x <- func
  case x of
    Nothing -> sequenceMaybeIO functions
    Just val -> do
      res <- sequenceMaybeIO functions
      return (val : res)

whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO func x = do
    y <- func x
    case y of
        Nothing  -> return()
        Just arg -> whileJustIO func arg

iforIO_:: [a] -> (Int->a-> IO())-> IO()
iforIO_ list func = Func list 0 func
    where
       Func(x:xs) i func = do 
            func i x 
            showFunc xs (i+1) func
       showFunc[] i func = return ()

example = do
    iforIO_ [1, 2] (\i n ->
        iforIO_ "ab" (\j c ->
           print ((i, j), replicate n c)))