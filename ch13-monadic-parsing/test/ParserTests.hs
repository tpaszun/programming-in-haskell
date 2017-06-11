module Main where

import Test.Hspec

import Data.Tree.Pretty

import Parser

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "should parse \"1+2+3\"" $ do
      let tree = eval "1+2+3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ESum
          (ESum
            (ETerm
              (TFactor
                (FExp
                  (Num 1))))
            (TFactor
              (FExp
                (Num 2))))
          (TFactor
            (FExp
              (Num 3))))
    it "should parse \"1-2-3\"" $ do
      let tree = eval "1-2-3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (EDif
          (EDif
            (ETerm
              (TFactor
                (FExp
                  (Num 1))))
            (TFactor
              (FExp
                (Num 2))))
          (TFactor
            (FExp
              (Num 3))))
    it "should parse \"1-2+3\"" $ do
      let tree = eval "1-2+3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ESum
          (EDif
            (ETerm
              (TFactor
                (FExp
                  (Num 1))))
            (TFactor
              (FExp
                (Num 2))))
          (TFactor
            (FExp
              (Num 3))))
    it "should parse \"1+2-3\"" $ do
      let tree = eval "1+2-3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (EDif
          (ESum
            (ETerm
              (TFactor
                (FExp
                  (Num 1))))
            (TFactor
              (FExp
                (Num 2))))
          (TFactor
            (FExp
              (Num 3))))
    it "should parse \"1-(2*3)-4\"" $ do
      let tree = eval "1-(2*3)-4"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (EDif
          (EDif
            (ETerm
              (TFactor
                (FExp
                  (Num 1))))
            (TFactor
              (FExp
                (EExpr
                  (ETerm
                    (TMul
                      (TFactor
                        (FExp
                          (Num 2)))
                      (FExp
                        (Num 3))))))))
          (TFactor
            (FExp
              (Num 4))))
    it "should parse \"1/2/3\"" $ do
      let tree = eval "1/2/3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ETerm
          (TDiv
            (TDiv
              (TFactor
                (FExp
                  (Num 1)))
              (FExp
                (Num 2)))
            (FExp
              (Num 3))))
    it "should parse \"1^2^3\"" $ do
      let tree = eval "1^2^3"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ETerm
          (TFactor
            (FExpo
              (Num 1)
              (FExpo
                (Num 2)
                (FExp
                  (Num 3))))))
    it "should parse \"1^(2+3)^4\"" $ do
      let tree = eval "1^(2+3)^4"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ETerm
          (TFactor
            (FExpo
              (Num 1)
              (FExpo
                (EExpr
                  (ESum
                    (ETerm
                      (TFactor
                        (FExp
                          (Num 2))))
                    (TFactor
                      (FExp
                        (Num 3)))))
                (FExp
                  (Num 4))))))
    it "should parse \"2^3*4\"" $ do
      let tree = eval "2^3*4"
      putStrLn $ drawVerticalTree $ exprToTree $ tree
      putStrLn $ drawVerticalTree $ exprToTree' $ tree
      tree `shouldBe`
        (ETerm
          (TMul
            (TFactor
              (FExpo
                (Num 2)
                (FExp
                  (Num 3))))
            (FExp
              (Num 4))))