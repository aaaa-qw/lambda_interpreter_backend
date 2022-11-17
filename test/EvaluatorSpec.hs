{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module EvaluatorSpec (spec) where

    import Test.Hspec ( describe, it, shouldBe, Spec )
    import Evaluator (replaceUnbound)
    import Parser (Expr(..), Program(..), parse)
    import qualified Data.Map as Map

    getExpr :: String -> Expr
    getExpr s = case parse s of
        Right (ProgE e) -> e
        Right (Decl _ e) -> e

    tableSym :: Map.Map String Expr
    tableSym = Map.fromList [("a", Id "x'0" ENoCnt), ("b", Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt), 
        ("c", Fun ["x"] (Id "x'0" (Fun ["y"] (Id "x'1" (Id "x" (Id "y" ENoCnt))) ENoCnt)) ENoCnt)]

    spec :: Spec
    spec = do
        describe "Evaluator.replaceUnbound" $ do
            it "empty expression; no definition" $ do
                replaceUnbound (getExpr "") Map.empty `shouldBe` ENoCnt

            it "All unbound variable expression; no definition" $ do
                replaceUnbound (getExpr "(a b a)") Map.empty `shouldBe` E (Id "x'0" (Id "x'1" (Id "x'2" ENoCnt))) ENoCnt
            
            it "All bounded variable; no definition" $ do
                replaceUnbound (getExpr "(lambda x y z.z(lambda a z.a x y z))") Map.empty 
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt
            
            it "Bounded + Unbounded variable; no definition" $ do
                replaceUnbound (getExpr "(lambda x y.z(lambda a z.a x y z)x a)x y z") Map.empty 
                `shouldBe` 
                Fun ["x", "y"] (Id "x'0" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) (Id "x" (Id "x'1" ENoCnt)))) (Id "x'2" (Id "x'3" (Id "x'4" ENoCnt)))

            it "empty expression; with definition" $ do
                replaceUnbound (getExpr "") tableSym `shouldBe` ENoCnt

            it "All bounded variable; with definition" $ do
                replaceUnbound (getExpr "(lambda x y z.z(lambda a z.a x y z))") tableSym
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt
            
            it "All unbound; all variable already defined in table symbol" $ do
                replaceUnbound (getExpr "(a b a)") tableSym `shouldBe` 
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) ENoCnt))) ENoCnt
            
            it "All unbound; some variable not defined in table symbol" $ do
                replaceUnbound (getExpr "(a b a d)") tableSym `shouldBe` 
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) (Id "x'2" ENoCnt)))) ENoCnt
            
            it "Bounded + Unbounded; some unbounded already defined in table symbol" $ do
                replaceUnbound (getExpr "(lambda x.x a c d)") tableSym `shouldBe`
                    Fun ["x"] (Id "x" (E (Id "x'0" ENoCnt) 
                        (E 
                            (Fun ["x"] (Id "x'1" (Fun ["y"] (Id "x'2" (Id "x" (Id "y" ENoCnt))) ENoCnt)) ENoCnt) 
                            (Id "x'3" ENoCnt))) ) ENoCnt