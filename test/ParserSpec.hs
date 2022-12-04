module ParserSpec (spec) where
    import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy )
    import Parser ( parse, Expr(ENoCnt, E, Fun, Id), Program(ProgE, Decl), unParse )
    import Data.Either (isLeft)
    
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
            
            it "Missing close parentheses" $ do
                parse "(a (b c)" `shouldSatisfy` isLeft

            it "Missing open parentheses" $ do
                parse "(a b c) d)a b)" `shouldSatisfy` isLeft
            
            it "Empty expression inside parenthesis fail to parse" $ do
                parse "(a b()c)" `shouldSatisfy` isLeft

            it "Function without body should fail to parse" $ do
                parse "(lambda x.)" `shouldSatisfy` isLeft
            
        describe "Parser.unParse" $ do
            it "Unparsing empty expression" $ do 
                unParse ENoCnt `shouldBe` ""
            
            it "Unparsing application only" $ do
                unParse (Id "a" (Id "b" (Id "c" ENoCnt))) `shouldBe` "a b c"
            
            it "Unparsing simple function only" $ do
                unParse (Fun ["x"] (Id "x" (Id "a" ENoCnt)) ENoCnt) `shouldBe` "(\955x.x a)"
            
            it "Unparsing nested function" $ do
                unParse (Fun ["x", "y"] (Id "x" (Fun ["a"] (Id "a" (Id "y" ENoCnt)) ENoCnt)) ENoCnt) `shouldBe`
                    "(\955x y.x(\955a.a y))"
            
            it "Unparsing function with application" $ do
                unParse (Id "a" (Fun ["x"] (Id "x" ENoCnt) (Id "b" ENoCnt))) `shouldBe` "a(\955x.x)b"
            
            it "Unparsing expression with important parenthesis" $ do
                unParse (Id "a" (E (Id "b" (Id "c" ENoCnt)) (Id "d" ENoCnt))) `shouldBe` "a(b c)d"
            
            it "Unparsing expression with unnecessary parenthesis" $ do
                unParse (Id "a" (E (Id "b" (Id "c" ENoCnt)) (Id "d" (E (Id "e" ENoCnt) (Id "f" ENoCnt))))) `shouldBe` 
                    "a(b c)d e f"
            
            it "Unparsing expression with unnecessary parenthesis II" $ do
                unParse (Id "a" (E (Id "b" (Id "c" ENoCnt)) (Id "d" (E (E (Id "e" ENoCnt) ENoCnt) (Id "f" ENoCnt))))) `shouldBe` 
                    "a(b c)d e f"