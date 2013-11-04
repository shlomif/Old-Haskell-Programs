getName :: IO String
getName = do
        putStr "Please enter your name: "
        name <- getLine
        putStrLn "Thank you.  Please wait."
        return name

main :: IO ()
main = do
    a <- getName
    putStrLn ("Hello " ++ a)
