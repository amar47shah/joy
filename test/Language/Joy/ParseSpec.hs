module Language.Joy.ParseSpec where

import           Language.Joy.Parser
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parsing numbers" $ do
        it "correctly parses valid input" $ do
            (parseJoy "1 2 3") `shouldBe` (Right [JoyNumber 1, JoyNumber 2, JoyNumber 3])

    describe "parsing symbols" $ do
        it "correctly parses valid input" $ do
            (parseJoy "dup") `shouldBe` (Right [JoySymbol "dup"])
            (parseJoy "+") `shouldBe` (Right [JoySymbol "+"])

    describe "parsing strings" $ do
        it "correctly parses valid input" $ do
            (parseJoy "\"HELLO\"") `shouldBe` (Right $ [JoyString "HELLO"])

    describe "parsing comments" $ do
        it "correctly parses valid input" $ do
            let expected = [JoyComment "Hello, World!"]
            (parseJoy "(* Hello, World! *)") `shouldBe` (Right expected)

    describe "parsing assignment" $ do
        it "correctly parses valid input" $ do
            let input = "let square == dup *;"
                expected = [JoyAssignment "square" [JoySymbol "dup",JoySymbol "*"]]
            (parseJoy input) `shouldBe` (Right expected)
