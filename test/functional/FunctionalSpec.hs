module FunctionalSpec (spec) where

import qualified Data.Vector.Unboxed as V
import qualified Finch
import Test.Hspec

spec :: Spec
spec = do
    it "do tests" $ do
        1 `shouldBe` 1
        -- need to abstract away the IO first...
        --let input = unlines ["aoeu"
                            --,""
                            --]
        --Finch.runString input
