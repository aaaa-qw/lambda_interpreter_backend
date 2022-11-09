module LexicalAnalyzerSpec where

    import Test.Hspec ( describe, it, shouldBe, Spec )
    import LexicalAnalyzer (autoParentheses)
    
    spec :: Spec
    spec = do
        describe "LexicalAnalyzer.autoParentheses" $ do
            it "empty string" $ do
                autoParentheses "" `shouldBe` "()"
            it "non empty expression" $ do
                autoParentheses "a b(lambda x . x) d" `shouldBe` "(a b(lambda x . x) d)"

            it "declaration" $ do
                autoParentheses "let newVar =  a b (lambda d. (lambda x.x) d)c" `shouldBe` "let newVar =(a b (lambda d. (lambda x.x) d)c)"  