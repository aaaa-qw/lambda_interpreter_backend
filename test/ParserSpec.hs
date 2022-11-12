module ParserSpec (spec) where
    import Test.Hspec ( describe, it, shouldBe, Spec )
    import Parser ( parse, Expr(ENoCnt, E, Fun, Id), Program(ProgE, Decl) )
    
    spec :: Spec
    spec = do
        describe "Parser.parse" $ do
            it "Empty Expression" $ do 
                parse "" `shouldBe` Right (ProgE ENoCnt)

            it "Application only, no function" $ do
                parse "(a b c)" `shouldBe` 
                    Right (ProgE 
                        (E (Id "a"  (Id "b" (Id "c" ENoCnt))) ENoCnt))            
            
            it "One function only, no Application" $ do
                parse "(lambda x.x)" `shouldBe` 
                    Right (ProgE
                        (Fun ["x"] (Id "x" ENoCnt) ENoCnt))   
                                 
            it "Function with application" $ do
                parse "(a b(lambda x y.(x y z))d)" `shouldBe` 
                    Right (ProgE
                        (E (Id "a" (
                            Id "b" 
                            (Fun ["x", "y"] (E (Id "x" (Id "y" (Id "z" ENoCnt))) ENoCnt) (Id "d" ENoCnt)) 
                        )) ENoCnt))

            it "inner function only" $ do
                parse "(lambda x.(lambda y.x s y)c)" `shouldBe`
                    Right (ProgE 
                        (Fun ["x"] (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt)) ENoCnt))
            
            it "Parenthesis affect evaluation" $ do
                parse "(a b(c d)e(f g)h)" `shouldBe`
                    Right (ProgE
                        (E (Id "a" (Id "b" 
                            (
                                E (Id "c" (Id "d" ENoCnt)) (Id "e" 
                                    (E (Id "f" (Id "g" ENoCnt)) (Id "h" ENoCnt))
                                )
                            )
                        )) ENoCnt))
        
            it "Inner function with outer apllication" $ do
                parse "(a b(lambda x.(lambda y.x s y)c)def)" `shouldBe` 
                    Right (ProgE 
                        (E (Id "a" (Id "b" 
                            (Fun ["x"]  
                                (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt))
                            (Id "def" ENoCnt))
                        )) ENoCnt))
        
            it "Declaration with Expression" $ do
                parse "let newVar=(a b(lambda x.(lambda y.x s y)c)def)" `shouldBe`
                    Right (Decl "newVar"  
                        (E (Id "a" (Id "b" 
                            (Fun ["x"]  
                                (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt))
                            (Id "def" ENoCnt))
                        )) ENoCnt))

            it "Function with \"\\\" definition" $ do
                parse "(a b(\\x.(\\y.x s y)c)def)" `shouldBe` 
                    Right (ProgE 
                        (E (Id "a" (Id "b" 
                            (Fun ["x"]  
                                (Fun ["y"] (Id "x" (Id "s" (Id "y" ENoCnt))) (Id "c" ENoCnt))
                            (Id "def" ENoCnt))
                        )) ENoCnt))