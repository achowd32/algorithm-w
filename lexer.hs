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
        let tok = (if pref == "false" then TokLit (Boolean False) 
                   else if pref == "true" then TokLit (Boolean True)
                   else Invalid) in
        tok : lexAll suff


------------- PARSER ------------- 
parseExpr :: [Token] -> Node
parseExpr [] = Bad
parseExpr [TokLit n] = Lit n
parseExpr (TokLit n1 : TokOp b : rest) = BinOp b (Lit n1) (parseExpr rest)
parseExpr _ = Bad

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (parseExpr (lexAll s))
        _ -> putStrLn "wrong"
