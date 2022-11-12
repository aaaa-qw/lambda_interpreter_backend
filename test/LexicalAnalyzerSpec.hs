module LexicalAnalyzerSpec (spec) where

    import Test.Hspec ( describe, it, shouldBe, Spec )
    import LexicalAnalyzer (autoParentheses)
    
    spec :: Spec
    spec = do
        describe "LexicalAnalyzer.autoParentheses" $ do
            it "empty string" $ do
                autoParentheses "" `shouldBe` ""
            it "should enclosed expression inside parenthesis" $ do
                autoParentheses "a b(lambda x .x) d" `shouldBe` "(a b(lambda x.x)d)"
            it "should remove unnecessary whitespace" $ do
                autoParentheses "a  b  (  lambda   x   y   .   d ( e f ) s    t     )     g           " `shouldBe` "(a b(lambda x y.d(e f)s t)g)"
            it "declaration" $ do
                autoParentheses "     let   newVar     =    a    b   (   lambda   d   . (   lambda    x   .   x   ) d   )    c   e   f    " 
                    `shouldBe` "let newVar=(a b(lambda d.(lambda x.x)d)c e f)"  