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

    it "should duplicate top element of the stack" $ do
        let initialStack = [5, 2]
            newStack = execState S.duplicate initialStack
        newStack `shouldBe` [5, 5, 2]

    it "should swap top elements of the stack" $ do
        let initialStack = [5, 2, 3]
            newStack = execState S.swap initialStack
        newStack `shouldBe` [2, 5, 3]
