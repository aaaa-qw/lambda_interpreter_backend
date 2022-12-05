module TableSymbolParserSpec (spec) where
    import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy )
    import TableSymbolParser (parseExpression)
    import Grammar ( Expr(ENoCnt, Fun, E, Id))
    import Data.Either (isLeft)
    
    spec :: Spec
    spec = do
        describe "Parser.parse" $ do
            it "Empty Expression" $ do 
                parseExpression "" `shouldBe` Right ENoCnt

            it "Application only, no function" $ do
                parseExpression "(a b c)" `shouldBe` 
                    Right (E (Id "a"  (Id "b" (Id "c" ENoCnt))) ENoCnt)            
            
            it "One function only, no Application" $ do
                parseExpression "(lambda x.x)" `shouldBe` 
                    Right (Fun ["x"] (Id "x" ENoCnt) ENoCnt)   
                                 
            it "Function with application" $ do
                parseExpression "(a b(lambda x y.(x y z))d)" `shouldBe` 
                    Right (E (Id "a" (
                            Id "b" 
                            (Fun ["x", "y"] (E (Id "x" (Id "y" (Id "z" ENoCnt))) ENoCnt) (Id "d" ENoCnt)) 
                        )) ENoCnt)

            it "inner function only" $ do
                parseExpression "(lambda x.(lambda y.x s y)c)" `shouldBe`
                    Right (Fun ["x"] (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt)) ENoCnt)
            
            it "Parenthesis affect evaluation" $ do
                parseExpression "(a b(c d)e(f g)h)" `shouldBe`
                    Right (E (Id "a" (Id "b" 
                            (
                                E (Id "c" (Id "d" ENoCnt)) (Id "e" 
                                    (E (Id "f" (Id "g" ENoCnt)) (Id "h" ENoCnt))
                                )
                            )
                        )) ENoCnt)
        
            it "Inner function with outer apllication" $ do
                parseExpression "(a b(lambda x.(lambda y.x s y)c)def)" `shouldBe` 
                    Right 
                        (E (Id "a" (Id "b" 
                            (Fun ["x"]  
                                (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt))
                            (Id "def" ENoCnt))
                        )) ENoCnt)
        
            it "Function with \"\\\" definition" $ do
                parseExpression "(a b(\\x.(\\y.x s y)c)def)" `shouldBe` 
                    Right  (E (Id "a" (Id "b" 
                            (Fun ["x"]  
                                (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt))
                            (Id "def" ENoCnt))
                        )) ENoCnt)
            
            it "Missing close parentheses" $ do
                parseExpression "(a (b c)" `shouldSatisfy` isLeft

            it "Missing open parentheses" $ do
                parseExpression "(a b c) d)a b)" `shouldSatisfy` isLeft
            
            it "Empty expression inside parenthesis fail to parse" $ do
                parseExpression "(a b()c)" `shouldSatisfy` isLeft

            it "Function without body should fail to parse" $ do
                parseExpression "(lambda x.)" `shouldSatisfy` isLeft
            
            it "Should parse variabel containing apostrophe" $ do
                parseExpression "(x'1(x'2 a)b)" `shouldBe` Right (E (Id "x'1" (E (Id "x'2" (Id "a" ENoCnt)) (Id "b" ENoCnt))) ENoCnt)
            
            it "Should parse variabel containing apostrophe inside function body" $ do
                parseExpression "(a(lambda x.x'0 x'2)c)" `shouldBe` 
                    Right (E (Id "a" (Fun ["x"] (Id "x'0" (Id "x'2" ENoCnt)) (Id "c" ENoCnt))) ENoCnt)