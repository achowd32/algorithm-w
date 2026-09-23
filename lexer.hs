import System.IO
import System.Environment
import Text.Read

data Literal = Num Int | Boolean Bool deriving (Show)
data Op = PLUS | MINUS | OR | AND deriving (Show)

data Token = TokLit Literal | TokOp Op | Invalid deriving (Show)

data Node
    = Lit Literal
    | BinOp Op Node Node
    | Bad
    deriving (Show)

parse :: [Token] -> Node
parse [] = Bad
parse (Invalid : _) = Bad
parse [TokLit n] = Lit n
parse (TokLit n : TokOp o : rest) = BinOp o (Lit n) (parse rest)
parse _ = Bad

lexOne :: String -> Token
lexOne str = 
    case (readMaybe str :: Maybe Int) of
       Just x -> TokLit (Num x)
       _ -> case str of
               "true" -> TokLit (Boolean True)
               "false" -> TokLit (Boolean False)
               "-" -> TokOp MINUS
               "+" -> TokOp PLUS
               "|" -> TokOp OR
               "&" -> TokOp AND
               _ -> Invalid

lexAll :: [String] -> [Token]
lexAll raw = map lexOne raw 

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (parse (lexAll (words s)))
        _ -> putStrLn "wrong"
