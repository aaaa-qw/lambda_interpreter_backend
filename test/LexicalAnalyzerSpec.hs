module LexicalAnalyzerSpec where

    import Test.Hspec ( describe, it, shouldBe, Spec )
    import LexicalAnalyzer (autoParentheses)
    
    spec :: Spec
    spec = do
        describe "LexicalAnalyzer.autoParentheses" $ do
            it "Application only, no function" $ do
                autoParentheses "a b c" `shouldBe` "(a b c)"
            
            it "One function only, no Application" $ do
                autoParentheses "lambda x . x" `shouldBe` "(lambda x . (x))"
            
            it "Function with application" $ do
                autoParentheses "a b (lambda x y . x y z) d" `shouldBe` "(a b (lambda x y . (x y z)) d)"

            it "Contains inner function" $ do
                autoParentheses "a b (lambda x . y (lambda x . x s y)) def" `shouldBe` "(a b (lambda x . (y (lambda x . (x s y)))) def)"

            it "Declaration with Expression" $ do
                autoParentheses "  Let   newVar   =   a b (lambda x y . y(x)y)lambda d . d(s)" 
                        `shouldBe` "Let newVar = (a b (lambda x y . (y(x)y))lambda d . (d(s)))"
            
            it "Does not change empty string" $ do
                autoParentheses "" `shouldBe` ""
            
            it "should remove excessive whitespace" $ do
                autoParentheses "    a   b    c    lambda x . x   " `shouldBe` "(a b c lambda x . (x))"
        
            it "white space only" $ do
                autoParentheses "       " `shouldBe` ""