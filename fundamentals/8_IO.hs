{-
IO
-}

{-
# IO
-}


{-
putStrLn :: String -> IO ()
IO side-effect action, with () as dummy return type
The empty tuple is a value of () and it also has a type of ().
-}

-- | `main` function will be executed if loaded

main0 :: IO ()
main0 = putStrLn "hello, world"


-- execute this will error: no show
act1 :: [IO ()]
act1 = [
  putStrLn "1",
  putStrLn "2"
  ]

-- need to use `do`
crunch :: [IO ()] -> IO ()
crunch [] = return;
crunch (x:xs) = do
  x
  crunch xs

{-
getLine :: IO String
IO action, with String as return type
The entire type of getLine is :: IO String
<-: get the value out of an I/O action
We can only take the IO return result (<-) when we're in IO action;
thus Haskell manages to neatly separate the pure and impure parts of our code.
-}
main1 = do  -- do block, impure/tainted environment.
    putStrLn "Hello, what's your name?"
    name <- getLine  -- bound getLine return to name
    putStrLn ("Hey " ++ name ++ ", you rock!")
    -- In a do block, the last action cannot be bound to a name. More on monads


{-
`return` makes an I/O action out of a pure value.
-}
main2 = do
    line <- getLine
    if null line
        then return ()  -- nothing like imperative programming
        else do
            putStrLn $ reverseWords line
            main2

reverseWords :: String -> String
reverseWords = unwords . map reverse . words


main3 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b


{-
# Files and streams
-}


{-
# Command line arguments
-}


{-
# Randomness
-}

{-
# Bytestrings
-}

{-
# Exceptions
-}
