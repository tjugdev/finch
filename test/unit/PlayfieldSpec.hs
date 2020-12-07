module PlayfieldSpec (spec) where

import Test.Hspec
import Playfield
import qualified Data.Vector.Unboxed as V

testInputString = unlines ["12 v"
                          ,"@.+<"
                          ]

spec :: Spec
spec = do
    describe "creation" $ do
        it "should create empty playfield with specified dimensions" $ do
            let playfield = emptyPlayfield 3 5
            (V.toList $ raw playfield) `shouldBe` "               "
            (width playfield) `shouldBe` 3
            (height playfield) `shouldBe` 5

        it "should create playfield from string with specified dimensions" $ do
            let playfield = fromString 6 3 testInputString
            (V.toList $ raw playfield) `shouldBe` "12 v  @.+<        "
            (width playfield) `shouldBe` 6
            (height playfield) `shouldBe` 3

        it "should crop playfield to specified dimensions when creating from string" $ do
            let playfield = fromString 3 1 testInputString
            (V.toList $ raw playfield) `shouldBe` "12 "
            (width playfield) `shouldBe` 3
            (height playfield) `shouldBe` 1

    describe "getChar" $ do
        it "should get correct character when location is valid" $ do
            let playfield = fromString 6 3 testInputString
                ch = Playfield.getChar playfield (2, 1)
            ch `shouldBe` '+'

        it "should get correct character when location is invalid" $ do
            let playfield = fromString 6 3 testInputString
                ch = Playfield.getChar playfield (-1, -1)
            ch `shouldBe` ' '

    describe "putChar" $ do
        it "should put character when location is valid" $ do
            let playfield = fromString 6 3 testInputString
                newPlayfield = Playfield.putChar playfield (2, 1) '-'
            (V.toList $ raw newPlayfield) `shouldBe` "12 v  @.-<        "

        it "should do nothing when location is invalid" $ do
            let playfield = fromString 6 3 testInputString
                newPlayfield = Playfield.putChar playfield (-1, -1) '-'
            (V.toList $ raw newPlayfield) `shouldBe` (V.toList $ raw playfield)
