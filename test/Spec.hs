import Test.Hspec
import Parser

-- Lex and parse a source string in one step.
parseStr :: String -> Node
parseStr = parse . lexAll

main :: IO ()
main = hspec $ do
    describe "lexAll" $ do
        it "lexes numbers and operators" $
            lexAll "12 + 3" `shouldBe` [TokNum 12, TokOp PLUS, TokNum 3]

        it "lexes keywords and variables" $
            lexAll "let x = true in x"
                `shouldBe` [TokLet, TokVar "x", TokEq, TokBool True, TokIn, TokVar "x"]

        it "lexes abstractions and parens" $
            lexAll "(x . x)" `shouldBe` [TokLParen, TokVar "x", TokAbs, TokVar "x", TokRParen]

    describe "parse" $ do
        it "parses literals" $ do
            parseStr "42" `shouldBe` LitInt 42
            parseStr "false" `shouldBe` LitBool False
            parseStr "y" `shouldBe` LitVar "y"

        it "parses binary operators" $
            parseStr "1 + 2" `shouldBe` BinOp PLUS (LitInt 1) (LitInt 2)

        it "parses let expressions" $
            parseStr "let x = 1 in x" `shouldBe` LetIn "x" (LitInt 1) (LitVar "x")

        it "parses abstractions" $
            parseStr "x . x" `shouldBe` Abs "x" (LitVar "x")

        it "parses application" $
            parseStr "(x . x) 1" `shouldBe` App (Abs "x" (LitVar "x")) (LitInt 1)

        it "parses let-bound functions applied to arguments" $
            parseStr "let id = x . x in id 5"
                `shouldBe` LetIn "id" (Abs "x" (LitVar "x")) (App (LitVar "id") (LitInt 5))

        it "returns Bad for unbalanced parens" $
            parseStr "(1 + 2" `shouldBe` Bad

    -- Known bugs: marked pending with xit. Change to `it` once fixed.
    describe "known bugs" $ do
        xit "applies a function to multiple arguments" $
            parseStr "f a b" `shouldBe` App (App (LitVar "f") (LitVar "a")) (LitVar "b")

        xit "makes subtraction left-associative" $
            parseStr "1 - 2 - 3"
                `shouldBe` BinOp MINUS (BinOp MINUS (LitInt 1) (LitInt 2)) (LitInt 3)

        xit "binds application tighter than operators" $
            parseStr "f 1 + 2" `shouldBe` BinOp PLUS (App (LitVar "f") (LitInt 1)) (LitInt 2)
