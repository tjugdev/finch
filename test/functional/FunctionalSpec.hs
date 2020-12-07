module FunctionalSpec (spec) where

import Test.Hspec
import qualified Finch
import qualified Data.Vector.Unboxed as V

spec :: Spec
spec = do
    it "do tests" $ do
        1 `shouldBe` 1
        -- need to abstract away the IO first...
        --let input = unlines ["aoeu"
                            --,""
                            --]
        --Finch.runString input
