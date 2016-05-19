module Language.Joy.ParseSpec where

import           Language.Joy.Parser as P
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parsing numbers" $ do
        it "correctly parses valid input" $ do
            (P.testParser parseNumber "1") `shouldBe` (Right (JoyNumber 1))

    describe "parsing literals" $ do
        it "correctly parses valid input" $ do
            (P.testParser parseLiteral "dup") `shouldBe` (Right (JoyLiteral "dup"))
            (P.testParser parseLiteral "+") `shouldBe` (Right (JoyLiteral "+"))

    describe "parsing strings" $ do
        it "correctly parses valid input" $ do
            (P.testParser parseString "\"HELLO\"") `shouldBe` (Right (JoyString "HELLO"))

    describe "parsing comments" $ do
        it "correctly parses valid input" $ do
            let expected = JoyComment "Hello, World!"
            (P.testParser parseComment "(* Hello, World! *)") `shouldBe` (Right expected)

    describe "parsing assignment" $ do
        it "correctly parses valid input" $ do
            let input = "square == dup *"
                expected = (JoyAssignment "square" [JoyLiteral "dup",JoyLiteral "*"])
            (P.testParser P.parseAssignment input) `shouldBe` (Right expected)
