module Main where

import Test.Hspec

import Data.Tree.Pretty

import Parser

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "should parse \"1+2+3\"" $ do
      let tree = eval "1+2+3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Sum
          (Sum
            (Term
              (Factor
                (Primitive
                  (Num 1))))
            (Factor
              (Primitive
                (Num 2))))
          (Factor
            (Primitive
              (Num 3))))
    it "should parse \"1-2-3\"" $ do
      let tree = eval "1-2-3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Difference
          (Difference
            (Term
              (Factor
                (Primitive
                  (Num 1))))
            (Factor
              (Primitive
                (Num 2))))
          (Factor
            (Primitive
              (Num 3))))
    it "should parse \"1-2+3\"" $ do
      let tree = eval "1-2+3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Sum
          (Difference
            (Term
              (Factor
                (Primitive
                  (Num 1))))
            (Factor
              (Primitive
                (Num 2))))
          (Factor
            (Primitive
              (Num 3))))
    it "should parse \"1+2-3\"" $ do
      let tree = eval "1+2-3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Difference
          (Sum
            (Term
              (Factor
                (Primitive
                  (Num 1))))
            (Factor
              (Primitive
                (Num 2))))
          (Factor
            (Primitive
              (Num 3))))
    it "should parse \"1-(2*3)-4\"" $ do
      let tree = eval "1-(2*3)-4"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Difference
          (Difference
            (Term
              (Factor
                (Primitive
                  (Num 1))))
            (Factor
              (Primitive
                (Expression
                  (Term
                    (Product
                      (Factor
                        (Primitive
                          (Num 2)))
                      (Primitive
                        (Num 3))))))))
          (Factor
            (Primitive
              (Num 4))))
    it "should parse \"1/2/3\"" $ do
      let tree = eval "1/2/3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Term
          (Quotient
            (Quotient
              (Factor
                (Primitive
                  (Num 1)))
              (Primitive
                (Num 2)))
            (Primitive
              (Num 3))))
    it "should parse \"1^2^3\"" $ do
      let tree = eval "1^2^3"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Term
          (Factor
            (Power
              (Num 1)
              (Power
                (Num 2)
                (Primitive
                  (Num 3))))))
    it "should parse \"1^(2+3)^4\"" $ do
      let tree = eval "1^(2+3)^4"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Term
          (Factor
            (Power
              (Num 1)
              (Power
                (Expression
                  (Sum
                    (Term
                      (Factor
                        (Primitive
                          (Num 2))))
                    (Factor
                      (Primitive
                        (Num 3)))))
                (Primitive
                  (Num 4))))))
    it "should parse \"2^3*4\"" $ do
      let tree = eval "2^3*4"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Term
          (Product
            (Factor
              (Power
                (Num 2)
                (Primitive
                  (Num 3))))
            (Primitive
              (Num 4))))
    it "should parse \"1+(-1)\"" $ do
      let tree = eval "1+(-1)"
      putStrLn $ drawVerticalTree $ exprToTreeFull $ tree
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      tree `shouldBe`
        (Sum
          (Term
            (Factor
              (Primitive
                (Num 1))))
          (Factor
            (Primitive
              (Expression
                (Term
                  (Factor
                    (Primitive
                      (Negation
                        (Term
                          (Factor
                            (Primitive
                              (Num 1))))))))))))