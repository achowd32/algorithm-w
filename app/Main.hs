import System.IO
import System.Environment
import Text.Read
import qualified Data.HashMap.Strict as HM

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Op = PLUS | MINUS | OR | AND deriving (Show)
data Token
    = TokNum Int
    | TokBool Bool
    | TokOp Op
    | TokVar String
    | TokLet
    | TokIn
    | TokEq
    | Invalid
    deriving (Show)

data Node
    = LitInt Int
    | LitBool Bool
    | BinOp Op Node Node
    | LetIn String Node Node
    | Bad
    deriving (Show)

------------- LEXER ------------- 
-- span :: (a -> Bool) -> [a] -> ([a], [a])
strToKey = HM.fromList [("true", TokBool True),
                        ("false", TokBool False),
                        ("let", TokLet),
                        ("in", TokIn)]

getKeyword :: String -> Maybe Token
getKeyword str = HM.lookup str strToKey

lexAll :: String -> [Token]
lexAll str
    | null str = []
    | (head str == '+') =  TokOp PLUS : lexAll (tail str)
    | (head str == '-') =  TokOp MINUS : lexAll (tail str)
    | (head str == '|') =  TokOp OR : lexAll (tail str)
    | (head str == '&') =  TokOp AND : lexAll (tail str)
    | (head str == '=') =  TokEq : lexAll (tail str)
    | isSpace (head str) = lexAll (tail str)
    | isDigit (head str) =
        let (pref, suff) = span isDigit str in
        let tokInt = read pref :: Int in
            (TokNum tokInt) : (lexAll suff)
    | isAlpha (head str) = 
        let (pref, suff) = span isAlphaNum str in
        let tok = case (getKeyword pref) of
                    Just keyword -> keyword
                    Nothing -> TokVar pref in
        tok : lexAll suff


------------- PARSER ------------- 
parse :: [Token] -> Node
parse s = case (parseExpr s) of
            Just (node, []) -> node
            _ -> Bad

parseExpr :: [Token] -> Maybe (Node, [Token])
parseExpr (TokLet : TokVar varName : TokEq : rest) = do
    (varVal, rest2) <- parseExpr rest
    case rest2 of
      TokIn : next -> do
          (body, remaining) <- parseExpr next
          Just (LetIn varName varVal body, remaining)
      _ -> Nothing
parseExpr toks = parseBinop toks

parseBinop :: [Token] -> Maybe (Node, [Token])
parseBinop toks = do
    (left, rest) <- parseAtom toks
    case rest of
        TokOp op : next -> do
            (right, remaining) <- parseAtom next
            Just (BinOp op left right, remaining)
        _ -> Nothing

parseAtom :: [Token] -> Maybe (Node, [Token])
parseAtom (TokNum n : rest) = Just (LitInt n, rest)
parseAtom (TokBool b : rest) = Just (LitBool b, rest)
parseAtom _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (parse (lexAll s))
        _ -> putStrLn "wrong"
