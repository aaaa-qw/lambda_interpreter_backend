{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module EvaluatorSpec (spec) where

    import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy)
    import Evaluator (betaReduce, evaluate, EvaluationError(..), replaceUnboundVars, replaceDupFunParams)
    import Parser (parse)
    import Grammar (Program(..), Expr(..))
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
        describe "Evaluator.replaceUnboundVars" $ do
            it "empty expression; no definition" $ do
                replaceUnboundVars Map.empty (getExpr "") `shouldBe` ENoCnt

            it "All unbound variable expression; no definition" $ do
                replaceUnboundVars Map.empty (getExpr "(a b a)") `shouldBe` E (Id "x'0" (Id "x'1" (Id "x'2" ENoCnt))) ENoCnt

            it "All bounded variable; no definition" $ do
                replaceUnboundVars Map.empty (getExpr "(lambda x y z.z(lambda a z.a x y z))")
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt

            it "Bounded + Unbounded variable; no definition" $ do
                replaceUnboundVars Map.empty (getExpr "(lambda x y.z(lambda a z.a x y z)x a)x y z")
                `shouldBe`
                Fun ["x", "y"] (Id "x'0" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) (Id "x" (Id "x'1" ENoCnt)))) (Id "x'2" (Id "x'3" (Id "x'4" ENoCnt)))

            it "empty expression; with definition" $ do
                replaceUnboundVars tableSym (getExpr "") `shouldBe` ENoCnt

            it "All bounded variable; with definition" $ do
                replaceUnboundVars tableSym (getExpr "(lambda x y z.z(lambda a z.a x y z))")
                `shouldBe` Fun ["x", "y", "z"] (Id "z" (Fun ["a", "z"] (Id "a" (Id "x" (Id "y" (Id "z" ENoCnt)))) ENoCnt)) ENoCnt

            it "All unbound; all variable already defined in table symbol" $ do
                replaceUnboundVars tableSym (getExpr "(a b a)") `shouldBe`
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) ENoCnt))) ENoCnt

            it "All unbound; some variable not defined in table symbol" $ do
                replaceUnboundVars tableSym (getExpr "(a b a d)") `shouldBe`
                    E (E (Id "x'0" ENoCnt) (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (E (Id "x'1" ENoCnt) (Id "x'2" ENoCnt)))) ENoCnt

            it "Bounded + Unbounded; some unbounded already defined in table symbol" $ do
                replaceUnboundVars tableSym (getExpr "(lambda x.x a c d)") `shouldBe`
                    Fun ["x"] (Id "x" (E (Id "x'0" ENoCnt)
                        (E
                            (Fun ["x"] (Id "x'1" (Fun ["y"] (Id "x'2" (Id "x" (Id "y" ENoCnt))) ENoCnt)) ENoCnt)
                            (Id "x'3" ENoCnt))) ) ENoCnt

            it "Variable in Parameter Lists should not be substitued" $ do
                replaceUnboundVars tableSym (getExpr "(lambda a c.a b c)") `shouldBe`
                    Fun ["a", "c"] (Id "a" (E (Fun ["a", "b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (Id "c" ENoCnt))) ENoCnt

            it "Complex nested function" $ do
                replaceUnboundVars Map.empty (getExpr "(lambda x.(lambda x.(lambda x.(lambda x.x))))") `shouldBe`
                    Fun ["x"] (Fun ["x"] (Fun ["x"] (Fun ["x"] (Id "x" ENoCnt) ENoCnt) ENoCnt) ENoCnt) ENoCnt
            
            it "Function with duplicate parameter" $ do 
                replaceUnboundVars Map.empty (getExpr "(lambda x x y x.x)") `shouldBe`
                    Fun ["x", "x", "y", "x"] (Id "x" ENoCnt) ENoCnt

        describe "Evaluator.replaceDupFunParams" $ do
            it "Empty Expression" $ do
                replaceDupFunParams (getExpr "") `shouldBe` ENoCnt
            
            it "No Function" $ do
                replaceDupFunParams (getExpr "(a b c)") `shouldBe` E (Id "a" (Id "b" (Id "c" ENoCnt))) ENoCnt
            
            it "Function with distinct parameter" $ do
                replaceDupFunParams (getExpr "((\\a b.a b)c)") `shouldBe`
                    E (Fun ["a"] (Fun ["b"] (Id "a" (Id "b" ENoCnt)) ENoCnt) (Id "c" ENoCnt)) ENoCnt 

            it "Duplicated parameter with inner function" $ do
                replaceDupFunParams (getExpr "(\\a.a(\\a b.a b)c)") `shouldBe`
                    Fun ["a"] (Id "a" (Fun ["y'0"] (Fun ["b"] (Id "y'0" (Id "b" ENoCnt) ) ENoCnt) (Id "c" ENoCnt))) ENoCnt

        describe "Evaluator.betaReduce" $ do
            it "Empty Expression" $ do
                betaReduce 1000 (getExpr "") `shouldBe` Right ENoCnt

            it "Application only" $ do
                betaReduce 1000 (getExpr "(a b c)") `shouldBe` Right (E (Id "a" (Id "b" (Id "c" ENoCnt))) ENoCnt)

            it "Application only with parenthesis" $ do
                betaReduce 1000 (getExpr "(a b(c d)e)") `shouldBe`
                    Right (E (Id "a" (Id "b" (E (Id "c" (Id "d" ENoCnt)) (Id "e" ENoCnt)))) ENoCnt)

            it "Function only" $ do
                betaReduce 1000 (getExpr "(lambda x.x c)")  `shouldBe`
                    Right (Fun ["x"] (Id "x" (Id "c" ENoCnt)) ENoCnt)

            it "Function with variable as arguments" $ do
                betaReduce 1000 (getExpr "((lambda x.(lambda y.y x))a b)") `shouldBe`
                    Right (E (Id "b" (Id "a" ENoCnt)) ENoCnt)

            it "Function with Function as argument" $ do
                betaReduce 100 (getExpr "((lambda x.(lambda y.a x y))(lambda y.a y))") `shouldBe`
                    Right (Fun ["y"] (Id "a" (Fun ["z'0"] (Id "a" (Id "z'0" ENoCnt)) (Id "y" ENoCnt))) ENoCnt)

            it "Contains not reducible function" $ do
                betaReduce 100 (getExpr "(a b(lambda x.x)c)") `shouldBe`
                    Right (E (Id "a" (Id "b" (Fun ["x"] (Id "x" ENoCnt) (Id "c" ENoCnt)))) ENoCnt)

            it "Contains reducible and not reducible function" $ do
                betaReduce 100 (getExpr "(a b(lambda x.x)c((lambda x.x)d))") `shouldBe`
                    Right (E (Id "a" (Id "b" (Fun ["x"] (Id "x" ENoCnt) (Id "c" (Id "d" ENoCnt))))) ENoCnt)

            it "Remove unnecessary parentheses after function application" $ do
                betaReduce 100 (getExpr "(x((lambda a.a)b)c)") `shouldBe`
                    Right (E (Id "x" (Id "b" (Id "c" ENoCnt))) ENoCnt)

            it "Irreducible function inside Unnecessary parentheses" $ do
                betaReduce 1000 (getExpr "(a((lambda x.x))c)") `shouldBe`
                    Right (E (Id "a"(Fun ["x"] (Id "x" ENoCnt) (Id "c" ENoCnt))) ENoCnt)
            
            it "Reducible function inside uneccessary parentheses" $ do
                betaReduce 1000 (getExpr "(((lambda x.x))a)") `shouldBe` 
                    Right (Id "a" ENoCnt)

            it "Reach maximum evaluation" $ do
                betaReduce 1000 (getExpr "(lambda f.((lambda x.(f (x x)))(lambda x.(f (x x)))))(lambda x.x)") `shouldSatisfy` isLeft

            it "Irreducible Function that have reducible body" $ do
                betaReduce 1000 (getExpr "(a(lambda x.(lambda y.(lambda z.y z))b)c)") `shouldBe`
                    Right(E (Id "a" (Fun ["x"] (Fun ["z"] (Id "b" (Id "z" ENoCnt)) ENoCnt) (Id "c" ENoCnt))) ENoCnt)

            it "Reducible Function that have reducible body" $ do
                betaReduce 1000 (getExpr "((lambda x.(lambda y.(lambda z.y z))b)c)") `shouldBe`
                    Right(Fun ["z"] (Id "b" (Id "z" ENoCnt)) ENoCnt)
            
            it "Function application with argument enclosed in parenthesis" $ do
                betaReduce 100 (getExpr "((lambda x.(lambda a.a x))(b c)d)") `shouldBe`
                    Right (E (Id "d" (E (Id "b" (Id "c" ENoCnt)) ENoCnt)) ENoCnt) 
            
            it "Function application with body containing parentheses" $ do
                betaReduce 100 (getExpr "((lambda x.x(x x)x)a)") `shouldBe`
                    Right (E (Id "a" (E (Id "a" (Id "a" ENoCnt)) (Id "a" ENoCnt))) ENoCnt)
                    

        describe "Evaluator.evaluate" $ do
            it "Evaluate expression; no error" $ do
                evaluate 1000 Map.empty (ProgE (getExpr "(x((lambda a.a)b)c)")) `shouldBe`
                    Right (ProgE (E (Id "x'0" (Id "x'1" (Id "x'2" ENoCnt))) ENoCnt))

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
                    Left VariableAlreadyDefined