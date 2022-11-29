{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module EvaluatorSpec (spec) where

    import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy)
    import Evaluator (replaceUnbound, betaReduce, evaluate, EvaluationError(..))
    import Parser (Expr(..), Program(..), parse)
    import qualified Data.Map as Map
    import Data.Either (isLeft)

    getExpr :: String -> Expr
    getExpr s = case parse s of
        Right (ProgE e) -> e
        Right (Decl _ e) -> e

    tableSym :: Map.Map String Expr
    tableSym = Map.fromList [
        ("a", Id "x'0" ENoCnt),
        ("b", Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt),
        ("c", Fun ["x"] (Id "x'0" (Fun ["y"] (Id "x'1" (Id "x" (Id "y" ENoCnt))) ENoCnt)) ENoCnt)]

    spec :: Spec
    spec = do
        describe "Evaluator.replaceUnbound" $ do
            it "empty expression; no definition" $ do
                replaceUnbound Map.empty (getExpr "") `shouldBe` ENoCnt

            it "All unbound variable expression; no definition" $ do
                replaceUnbound Map.empty (getExpr "(a b a)") `shouldBe` E (Id "x'0" (Id "x'1" (Id "x'2" ENoCnt))) ENoCnt

            it "All bounded variable; no definition" $ do
                replaceUnbound Map.empty (getExpr "(lambda x y z.z(lambda a z.a x y z))")
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt

            it "Bounded + Unbounded variable; no definition" $ do
                replaceUnbound Map.empty (getExpr "(lambda x y.z(lambda a z.a x y z)x a)x y z")
                `shouldBe`
                Fun ["x", "y"] (Id "x'0" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) (Id "x" (Id "x'1" ENoCnt)))) (Id "x'2" (Id "x'3" (Id "x'4" ENoCnt)))

            it "empty expression; with definition" $ do
                replaceUnbound tableSym (getExpr "") `shouldBe` ENoCnt

            it "All bounded variable; with definition" $ do
                replaceUnbound tableSym (getExpr "(lambda x y z.z(lambda a z.a x y z))")
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt

            it "All unbound; all variable already defined in table symbol" $ do
                replaceUnbound tableSym (getExpr "(a b a)") `shouldBe`
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) ENoCnt))) ENoCnt

            it "All unbound; some variable not defined in table symbol" $ do
                replaceUnbound tableSym (getExpr "(a b a d)") `shouldBe`
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) (Id "x'2" ENoCnt)))) ENoCnt

            it "Bounded + Unbounded; some unbounded already defined in table symbol" $ do
                replaceUnbound tableSym (getExpr "(lambda x.x a c d)") `shouldBe`
                    Fun ["x"] (Id "x" (E (Id "x'0" ENoCnt)
                        (E
                            (Fun ["x"] (Id "x'1" (Fun ["y"] (Id "x'2" (Id "x" (Id "y" ENoCnt))) ENoCnt)) ENoCnt)
                            (Id "x'3" ENoCnt))) ) ENoCnt

            it "Variable in Parameter Lists should not be substitued" $ do
                replaceUnbound tableSym (getExpr "(lambda a c.a b c") `shouldBe`
                    Fun ["a", "c"] (Id "a" (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (Id "c" ENoCnt))) ENoCnt


        describe "Evaluator.betaReduce" $ do
            it "Empty Expression" $ do
                betaReduce 1000 (getExpr "") `shouldBe` Right ENoCnt

            it "Application only" $ do
                betaReduce 1000 (getExpr "(a b c)") `shouldBe` Right (Id "a" (Id "b" (Id "c" ENoCnt)))

            it "Application only with parenthesis" $ do
                betaReduce 1000 (getExpr "(a b(c d)e)") `shouldBe`
                    Right (Id "a" (Id "b" (E (Id "c" (Id "d" ENoCnt)) (Id "e" ENoCnt))))

            it "Function only" $ do
                betaReduce 1000 (getExpr "(lambda x.x c)")  `shouldBe`
                    Right (Fun ["x"] (Id "x" (Id "c" ENoCnt)) ENoCnt)

            it "Function with variable as arguments" $ do
                betaReduce 1000 (getExpr "((lambda x y.y x)a b)") `shouldBe`
                    Right (Id "b" (Id "a" ENoCnt))

            it "Function with Function as argument" $ do
                betaReduce 100 (getExpr "((lambda x y.x y)(lambda x.a x)b)") `shouldBe`
                    Right (Id "a" (Id "b" ENoCnt))

            it "Contains not reducible function" $ do
                betaReduce 100 (getExpr "(a b(lambda x.x)c)") `shouldBe`
                    Right (Id "a" (Id "b" (Fun ["x"] (Id "x" ENoCnt) (Id "c" ENoCnt))))

            it "Contains reducible and not reducible function" $ do
                betaReduce 100 (getExpr "(a b(lambda x.x)c((lambda x.x)d))") `shouldBe`
                    Right (Id "a" (Id "b" (Fun ["x"] (Id "x" ENoCnt) (Id "c" (Id "d" ENoCnt)))))

            it "Remove unnecessary parentheses after function application" $ do
                betaReduce 100 (getExpr "(x((lambda a.a)b)c)") `shouldBe`
                    Right (Id "x" (Id "b" (Id "c" ENoCnt)))

            it "Unnecessary parentheses" $ do
                betaReduce 1000 (getExpr "(((lambda x.x)))") `shouldBe`
                    Right (Fun ["x"] (Id "x" ENoCnt) ENoCnt)

            it "Reach maximum evaluation" $ do
                betaReduce 1000 (getExpr "(lambda f.((lambda x.(f (x x)))(lambda x.(f (x x)))))(lambda x.x)") `shouldSatisfy` isLeft

        describe "Evaluator.evaluate" $ do
            it "Evaluate expression; no error" $ do
                evaluate 1000 Map.empty (ProgE (getExpr "(x((lambda a.a)b)c)")) `shouldBe`
                    Right (ProgE (Id "x'0" (Id "x'1" (Id "x'2" ENoCnt))))

            it "Evaluate expression maximum evaluation exceeded" $ do
                evaluate 1000 Map.empty (ProgE (getExpr "(lambda f.((lambda x.(f (x x)))(lambda x.(f (x x)))))(lambda x.x)")) `shouldSatisfy`
                    (\(Left (ExceedMaxEval _)) -> True)

            it "Evaluate declaration; no error" $ do
                evaluate 1000 Map.empty (Decl "a" (getExpr "(((lambda x.x)))")) `shouldBe`
                    Right (Decl "a" (Fun ["x"] (Id "x" ENoCnt) ENoCnt))

            it "Evaluate declaration; evaluation reach maximum value" $ do
                evaluate 1000 Map.empty (Decl "a" (getExpr "(lambda f.((lambda x.(f (x x)))(lambda x.(f (x x)))))(lambda x.x)")) `shouldSatisfy`
                    (\(Left (ExceedMaxEval _)) -> True)

            it "Evaluate declaration; variable already defined" $ do
                evaluate 1000 (Map.fromList [("a", Id "b" ENoCnt)]) (Decl "a" (getExpr "(((lambda x.x)))")) `shouldBe`
                    Left (VariableAlreadyDefined $ "a" ++ " is already defined as " ++ "b")