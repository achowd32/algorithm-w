import System.IO
import System.Environment
import Text.Read

data Literal = Num Int | Boolean Bool
data Op = PLUS | MINUS
data Token = TokLit Literal | TokOp Op | Invalid

lexOne :: String -> Token
lexOne str = 
    case (readMaybe str :: Maybe Int) of
       Just x -> TokLit (Num x)
       _ -> case str of
               "true" -> TokLit (Boolean True)
               "false" -> TokLit (Boolean False)
               "-" -> TokOp MINUS
               "+" -> TokOp PLUS
               _ -> Invalid

lexAll :: [String] -> [Token]
lexAll raw = map lexOne raw 

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> lexAll (words s)
        _ -> putStrLn "wrong"
