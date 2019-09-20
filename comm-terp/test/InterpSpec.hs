module InterpSpec where

import Interp
import Syntax

import Test.Hspec
import qualified Data.Map as M

testRun :: Command -> (Either String String, Env)
testRun = exec' M.empty run

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should do nothing" $ fst (testRun Skip) `shouldBe` Right ""