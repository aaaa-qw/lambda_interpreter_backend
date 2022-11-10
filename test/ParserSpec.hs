module ParserSpec (spec) where
    import Test.Hspec ( describe, it, shouldBe, Spec )
    import Parser
    
    spec :: Spec
    spec = do
        describe "Parser.parse" $ do
            it "Empty Expression" $ do 
                parse "" `shouldBe` PNoCnt

            it "Application only, no function" $ do
                parse "(a b c)" `shouldBe` 
                    ProgE 
                        (AppE (IdE "a") 
                            (AppE' (IdE "b") 
                                (AppE' (IdE "c") E'NoCnt)))
            
            it "One function only, no Application" $ do
                parse "(lambda x . (x))" `shouldBe` 
                    ProgE
                        (AppE (Funct (Params ["x"]) (AppE (IdE "x") E'NoCnt) ) E'NoCnt)
            
            it "Function with application" $ do
                parse "(a b (lambda x y . (x y z)) d)" `shouldBe` 
                    ProgE
                        (AppE (IdE "a") 
                            (AppE' (IdE "b")
                                (AppE' (AppE (Funct (Params ["x", "y"]) (AppE (IdE "x") (AppE' (IdE "y") (AppE' (IdE "z") E'NoCnt)))) E'NoCnt) 
                                    (AppE' (IdE "d") E'NoCnt))))
            it "inner function only" $ do
                parse "(lambda x.((lambda y.(x s y))c))" `shouldBe`
                    ProgE 
                        (AppE 
                            (Funct (Params ["x"]) 
                                (AppE 
                                    (AppE (Funct (Params ["y"]) 
                                        (AppE (IdE "x") (AppE' (IdE "s") (AppE' (IdE "y") E'NoCnt)))) E'NoCnt) 
                                    (AppE' (IdE "c") E'NoCnt)
                                )
                            ) 
                            E'NoCnt
                        )

            it "Inner function with outer apllication" $ do
                parse "(a b (lambda x . ((lambda y . (x s y))c)) def)" `shouldBe` 
                    ProgE
                        (AppE (IdE "a") (AppE' (IdE "b") 
                        (
                            AppE'                         
                                (AppE 
                                    (Funct (Params ["x"]) 
                                        (AppE 
                                            (AppE (Funct (Params ["y"]) 
                                                (AppE (IdE "x") (AppE' (IdE "s") (AppE' (IdE "y") E'NoCnt)))) E'NoCnt) 
                                            (AppE' (IdE "c") E'NoCnt)
                                        )
                                    ) 
                                    E'NoCnt
                                )
                                (AppE' (IdE "def") E'NoCnt)
                        )))

            it "Declaration with Expression" $ do
                parse "let newVar = (a b (lambda x . ((lambda y . (x s y))c)) def)" `shouldBe`
                    Decl "newVar" 
                        (AppE (IdE "a") (AppE' (IdE "b") 
                        (
                            AppE'                         
                                (AppE 
                                    (Funct (Params ["x"]) 
                                        (AppE 
                                            (AppE (Funct (Params ["y"]) 
                                                (AppE (IdE "x") (AppE' (IdE "s") (AppE' (IdE "y") E'NoCnt)))) E'NoCnt) 
                                            (AppE' (IdE "c") E'NoCnt)
                                        )
                                    ) 
                                    E'NoCnt
                                )
                                (AppE' (IdE "def") E'NoCnt)
                        )))
