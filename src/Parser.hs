module Parser where

import qualified Data.HashMap.Strict as HM

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Op = PLUS | MINUS | OR | AND deriving (Show, Eq)
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
    deriving (Show, Eq)

data Node
    = LitInt Int
    | LitVar String
    | LitBool Bool
    | BinOp Op Node Node
    | LetIn String Node Node
    | Abs String Node
    | App Node Node
    | Bad
    deriving (Show, Eq)

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
parse s = case (parseLet s) of
            Just (node, []) -> node
            _ -> Bad

parseLet :: [Token] -> Maybe (Node, [Token])
parseLet (TokLet : TokVar varName : TokEq : rest) = do
    (varVal, rest2) <- parseLet rest
    case rest2 of
      TokIn : next -> do
          (body, remaining) <- parseLet next
          Just (LetIn varName varVal body, remaining)
      _ -> Nothing
parseLet toks = parseAbs toks

parseAbs :: [Token] -> Maybe (Node, [Token])
parseAbs (TokVar varName : TokAbs : rest) = do
    (absBody, rest2) <- parseLet rest
    Just (Abs varName absBody, rest2)
parseAbs toks = parseBinop toks

parseBinop :: [Token] -> Maybe (Node, [Token])
parseBinop toks = do
    (left, rest) <- parseApp toks
    case rest of
        TokOp op : next -> do
            (right, remaining) <- parseLet next
            Just (BinOp op left right, remaining)
        _ -> Just (left, rest)

parseApp :: [Token] -> Maybe (Node, [Token])
parseApp toks = do
    (first, rest) <- parseAtom toks
    Just (go first rest)
  where
    go acc ts = case parseAtom ts of
      Just (arg, rest') -> go (App acc arg) rest'
      Nothing           -> (acc, ts)

parseAtom :: [Token] -> Maybe (Node, [Token])
parseAtom (TokNum n : rest) = Just (LitInt n, rest)
parseAtom (TokBool b : rest) = Just (LitBool b, rest)
parseAtom (TokVar s : rest) = Just (LitVar s, rest)
parseAtom toks = parseParen toks

parseParen :: [Token] -> Maybe (Node, [Token])
parseParen (TokLParen : rest) = do
    (expr, remaining) <- parseLet rest
    case remaining of
      TokRParen : [] -> Just (expr, [])
      TokRParen : leftover -> Just (expr, leftover)
      _ -> Nothing
parseParen _ = Nothing
