import System.IO
import System.Environment
import Text.Read

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Literal = Num Int | Boolean Bool deriving (Show)
data Op = PLUS | MINUS | OR | AND deriving (Show)

data Token = TokLit Literal | TokOp Op | Invalid deriving (Show)

data Node
    = Lit Literal
    | BinOp Op Node Node
    | Bad
    deriving (Show)

------------- LEXER ------------- 
-- span :: (a -> Bool) -> [a] -> ([a], [a])

lexAll :: String -> [Token]
lexAll str
    | null str = []
    | (head str == '+') =  TokOp PLUS : lexAll (tail str)
    | (head str == '-') =  TokOp MINUS : lexAll (tail str)
    | (head str == '|') =  TokOp OR : lexAll (tail str)
    | (head str == '&') =  TokOp AND : lexAll (tail str)
    | isSpace (head str) = lexAll (tail str)
    | isDigit (head str) =
        let (pref, suff) = span isDigit str in
        let tokInt = read pref :: Int in
            (TokLit (Num tokInt)) : (lexAll suff)
    | isAlpha (head str) = 
        let (pref, suff) = span isAlphaNum str in
        if pref == "false" then TokLit (Boolean False) : lexAll suff
        else if pref == "true" then TokLit (Boolean True) : lexAll suff
        else Invalid : lexAll suff

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (lexAll s)
        _ -> putStrLn "wrong"
