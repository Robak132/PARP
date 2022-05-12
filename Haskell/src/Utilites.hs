module Utilites where
    import qualified Data.List as List

    printLines :: [String] -> IO ()
    printLines xs = putStr (unlines ("" : xs))

    split :: String -> [String]
    split s = case dropWhile (==' ') s of
        "" -> []
        s' -> w : split s''
            where (w, s'') = break (==' ') s'

    splitCommand :: String -> String
    splitCommand cmd = unwords (tail(split cmd))