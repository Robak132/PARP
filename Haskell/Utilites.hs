module Utilites where
    printLines :: [String] -> IO ()
    printLines xs = putStr (unlines xs)

    readCommand :: IO String
    readCommand = do getLine

    split :: String -> [String]
    split s = do
        case dropWhile (==' ') s of
            "" -> []
            s' -> w : split s''
                where (w, s'') = break (==' ') s'