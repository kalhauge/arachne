{-# LANGUAGE BlockArguments #-}
module Arachne.GraphTSpec where

import Data.Foldable
import Control.Applicative

import Test.Hspec
import Arachne.GraphT

spec :: Spec
spec = do
  describe "Easy Peasy" do
    it "can equate two strings" do
      (a, b) <- drawGraphT do
        nodeG (return ())
        nodeG (return ())
        for_ [0, 10, 12] \i ->
          nodeG (return i)
        nodeG (return ())
      a `shouldBe` ()
      b `shouldBe` ["ehll"]

