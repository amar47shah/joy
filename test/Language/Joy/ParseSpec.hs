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
    describe "parsing assignment" $ do
        it "correctly parses valid input" $ do
            let input = "square == dup *"
                expected = (JoyAssignment "square" [JoyLiteral "dup",JoyLiteral "*"])
            (P.testParser P.parseAssignment input) `shouldBe` (Right expected)
