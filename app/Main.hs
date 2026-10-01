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
    | TokLParen
    | TokRParen
    | TokAbs
    | Invalid
    deriving (Show)

data Node
    = LitInt Int
    | LitVar String
    | LitBool Bool
    | BinOp Op Node Node
    | LetIn String Node Node
    | Abs String Node
    | App Node Node
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
    | (head str == '(') = TokLParen : lexAll (tail str)
    | (head str == ')') = TokRParen : lexAll (tail str)
    | (head str == '.') = TokAbs : lexAll (tail str)
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
parse s = case (parseApp s) of
            Just (node, []) -> node
            _ -> Bad

parseApp :: [Token] -> Maybe (Node, [Token])
parseApp toks = do
    (expr1, rest1) <- parseExpr toks
    case (parseExpr rest1) of
      Just (expr2, rest2) -> Just (App expr1 expr2, rest2)
      Nothing -> Just (expr1, rest1)

parseExpr :: [Token] -> Maybe (Node, [Token])
parseExpr (TokLet : TokVar varName : TokEq : rest) = do
    (varVal, rest2) <- parseApp rest
    case rest2 of
      TokIn : next -> do
          (body, remaining) <- parseApp next
          Just (LetIn varName varVal body, remaining)
      _ -> Nothing
parseExpr toks = parseAbs toks

parseAbs :: [Token] -> Maybe (Node, [Token])
parseAbs (TokVar varName : TokAbs : rest) = do
    (absBody, rest2) <- parseApp rest
    Just (Abs varName absBody, rest2)
parseAbs toks = parseBinop toks

parseBinop :: [Token] -> Maybe (Node, [Token])
parseBinop toks = do
    (left, rest) <- parseAtom toks
    case rest of
        TokOp op : next -> do
            (right, remaining) <- parseApp next
            Just (BinOp op left right, remaining)
        _ -> Just (left, rest)

parseAtom :: [Token] -> Maybe (Node, [Token])
parseAtom (TokNum n : rest) = Just (LitInt n, rest)
parseAtom (TokBool b : rest) = Just (LitBool b, rest)
parseAtom (TokVar s : rest) = Just (LitVar s, rest)
parseAtom toks = parseParen toks

parseParen :: [Token] -> Maybe (Node, [Token])
parseParen (TokLParen : rest) = do
    (expr, remaining) <- parseApp rest
    case remaining of
      TokRParen : [] -> Just (expr, [])
      TokRParen : leftover -> Just (expr, leftover)
      _ -> Nothing
parseParen _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> print (parse (lexAll s))
        _ -> putStrLn "wrong"
