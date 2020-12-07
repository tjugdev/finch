module PlayfieldSpec (spec) where

import Test.Hspec
import Finch
import qualified Data.Vector.Unboxed as V

spec :: Spec
spec = do
    describe "creation" $ do
        it "should create empty playfield with specified dimensions" $ do
            let pf = emptyPlayfield 3 5
            (V.toList $ source pf) `shouldBe` "               "
            (width pf) `shouldBe` 3
            (height pf) `shouldBe` 5

        it "should create playfield from string with specified dimensions" $ do
            let input = "12 v\n@.+<"
                pf = playfieldFromString input 6 3
            (V.toList $ source pf) `shouldBe` "12 v  @.+<        "
            (width pf) `shouldBe` 6
            (height pf) `shouldBe` 3

        it "should crop playfield to specified dimensions when creating from string" $ do
            let input = "12 v\n@.+<"
                pf = playfieldFromString input 3 1
            (V.toList $ source pf) `shouldBe` "12 "
            (width pf) `shouldBe` 3
            (height pf) `shouldBe` 1
