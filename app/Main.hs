import System.Environment
import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (parse (lexAll s))
        _ -> putStrLn "wrong"
