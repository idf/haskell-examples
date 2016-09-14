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
main0 = putStrLn "hello, world"

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
