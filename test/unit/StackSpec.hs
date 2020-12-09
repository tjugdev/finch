module StackSpec (spec) where

import Control.Monad.State
import qualified Stack as S
import Test.Hspec

spec :: Spec
spec = do
    it "should pop element off stack" $ do
        let initialStack = [1, 2, 3]
            (result, newStack) = runState S.pop initialStack
        result `shouldBe` 1
        newStack `shouldBe` [2, 3]

    it "should pop zero off an empty stack" $ do
        let initialStack = []
            (result, newStack) = runState S.pop initialStack
        result `shouldBe` 0
        newStack `shouldBe` []

    it "should push an element onto the stack" $ do
        let initialStack = [1, 2, 3]
            newStack = execState (S.push 5) initialStack
        newStack `shouldBe` [5, 1, 2, 3]

    it "should add elements on the stack" $ do
        let initialStack = [5, 6, 7]
            newStack = execState S.add initialStack
        newStack `shouldBe` [11, 7]

    it "should subtract elements on the stack" $ do
        let initialStack = [5, 6, 7]
            newStack = execState S.subtract initialStack
        newStack `shouldBe` [1, 7]

    it "should multiply elements on the stack" $ do
        let initialStack = [5, 6, 7]
            newStack = execState S.multiply initialStack
        newStack `shouldBe` [30, 7]

    it "should (integer) divide elements on the stack" $ do
        let initialStack = [5, 31, 7]
            newStack = execState S.divide initialStack
        newStack `shouldBe` [6, 7]

    it "should compute modulo of elements on the stack" $ do
        let initialStack = [5, 31, 7]
            newStack = execState S.modulo initialStack
        newStack `shouldBe` [1, 7]

    it "should duplicate top element of the stack" $ do
        let initialStack = [5, 2]
            newStack = execState S.duplicate initialStack
        newStack `shouldBe` [5, 5, 2]

    it "should swap top elements of the stack" $ do
        let initialStack = [5, 2, 3]
            newStack = execState S.swap initialStack
        newStack `shouldBe` [2, 5, 3]

    it "should compute not for non-zero element" $ do
        let initialStack = [5, 2, 3]
            newStack = execState S.not initialStack
        newStack `shouldBe` [0, 2, 3]

    it "should compute not for zero element" $ do
        let initialStack = [0, 2, 3]
            newStack = execState S.not initialStack
        newStack `shouldBe` [1, 2, 3]

    it "should compute greaterThan when top is larger" $ do
        let initialStack = [5, 2, 3]
            newStack = execState S.greaterThan initialStack
        newStack `shouldBe` [0, 3]

    it "should compute greaterThan when top is smaller" $ do
        let initialStack = [2, 5, 3]
            newStack = execState S.greaterThan initialStack
        newStack `shouldBe` [1, 3]
