module FunctionalSpec (spec) where

import Control.Monad.State (execState)
import Data.List (intersperse)
import qualified Data.Vector.Unboxed as V
import Finch (runStringStandardSize_)
import FinchIO (MockIO, makeMockIO, mockStdout)
import Test.Hspec

runProgram :: String -> String -> [Int] -> String
runProgram src stdin randomStream = mockStdout $ execState result initialIO
  where
    result    = runStringStandardSize_ src
    initialIO = makeMockIO stdin randomStream

spec :: Spec
spec = do
    it "should add handle basic arithmetic" $ do
        let input = "12+.51-.32*.62/.73%.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "3 4 6 3 1 "

    it "should handle not" $ do
        let input = "0!.2!.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "1 0 "

    it "should handle greater than" $ do
        let input = "12`.21`.11`.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "0 1 0 "

    it "should handle handle movement commands" $ do
        let input = unlines ["v@....1<"
                            ,"4      2"
                            ,">   3  ^"
                            ]
            stdout = runProgram input "" []
        stdout `shouldBe` "1 2 3 4 "

    it "should wrap movement" $ do
        let input = unlines ["^      2"
                            ,"1....@ >"
                            ,"<      v3"
                            ,"4"
                            ]
            stdout = runProgram input "" []
        stdout `shouldBe` "1 2 3 4 "

    it "should handle move random command" $ do
        let input = unlines ["?@....1?"
                            ,"4      2"
                            ,"?   3  ?"
                            ]
            stdout = runProgram input "" [7, 17, -2, 0]
        stdout `shouldBe` "1 2 3 4 "

    it "should handle horizontal if command" $ do
        let input = "\"tseT\">:#,_@"
            stdout = runProgram input "" []
        stdout `shouldBe` "Test"

    it "should handle vertical if command" $ do
        let input = intersperse '\n' "v\"tseT\"v:#,|@"
            stdout = runProgram input "" []
        stdout `shouldBe` "Test"

    it "should handle string mode" $ do

        let input = "\"tseT\",,,,@"
            stdout = runProgram input "" []
        stdout `shouldBe` "Test"

    it "should handle duplicate command" $ do
        let input = "2:..@"
            stdout = runProgram input "" []
        stdout `shouldBe` "2 2 "

    it "should handle swap command" $ do
        let input = "12\\..@"
            stdout = runProgram input "" []
        stdout `shouldBe` "1 2 "

    it "should handle discard command" $ do
        let input = "123.$.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "3 1 "

    it "should be able to print as integer" $ do
        let input = "66*.06-6*.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "36 -36 "

    it "should be able to print as character" $ do
        let input = "74+3*,@"
            stdout = runProgram input "" []
        stdout `shouldBe` "!"

    it "should handle bridge command" $ do
        let input = "1#23+.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "4 "

    it "should handle get and put commands" $ do
        -- Reads the 3 and writes it over top of the 9
        let input = unlines ["01g60p95+.@"
                            ,"3"
                            ]
            stdout = runProgram input "" []
        stdout `shouldBe` "8 "

    it "should read an integer from the user" $ do
        let input = "&&+.@"
            stdout = runProgram input "12\n-7\n" []
        stdout `shouldBe` "5 "

    it "should read a character from the user" $ do
        let input = "~84*+,@"
            stdout = runProgram input "F" []
        stdout `shouldBe` "f"

    it "should return zero when trying to pop an element off an empty stack" $ do
        let input = ".1+.@"
            stdout = runProgram input "" []
        stdout `shouldBe` "0 1 "

    it "should push integers onto the stack" $ do
        let input = "9876543210..........@"
            stdout = runProgram input "" []
        stdout `shouldBe` "0 1 2 3 4 5 6 7 8 9 "
