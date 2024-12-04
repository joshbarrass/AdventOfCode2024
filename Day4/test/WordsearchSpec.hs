module Main where

import Test.Hspec
import Wordsearch

main :: IO ()
main = hspec $ do
  let wordsearch = ["ABCDEFGH"
                   ,"IJKLMNOP"
                   ,"QRSTUVWX"
                   ,"YZ123456"
                   ,"7890abcd"
                   ,"efghijkl"
                   ,"mnopqrst"
                   ,"uvwxyz@#"
                   ,"!£$%^&*("] :: Wordsearch
  let coord = (3, 2) -- 'T'

  describe "width" $ do
    it "returns the width" $ do
      (width wordsearch) `shouldBe` 8

  describe "height" $ do
    it "returns the height" $ do
      (height wordsearch) `shouldBe` 9

  describe "!.!" $ do
    it "returns the character at coord" $ do
      (wordsearch !.! coord) `shouldBe` 'T'

  describe "!-!" $ do
    it "returns the row" $ do
      (wordsearch !-! 1) `shouldBe` "IJKLMNOP"

  describe "!|!" $ do
    it "returns the column" $ do
      (wordsearch !|! 1) `shouldBe` "BJRZ8fnv£"

  describe "!\\!" $ do
    it "returns the diagonal" $ do
      (wordsearch !\! coord) `shouldBe` "BKT3bkt"
      (wordsearch !\! (width wordsearch - 1, 0)) `shouldBe` "H"
      (wordsearch !\! (0, height wordsearch -1)) `shouldBe` "!"
      (wordsearch !\! (1, 7)) `shouldBe` "mv$"
      (wordsearch !\! (7, 7)) `shouldBe` "AJS2ajs#"
      (wordsearch !\! (6, 7)) `shouldBe` "IR10ir@("
      (wordsearch !\! (6, 1)) `shouldBe` "FOX"
      (wordsearch !\! (6, 2)) `shouldBe` "ENW6"

  describe "!/!" $ do
    it "returns the diagonal" $ do
      (wordsearch !/! coord) `shouldBe` "e81TMF"
      (wordsearch !/! (width wordsearch - 1, height wordsearch -1)) `shouldBe` "("
      (wordsearch !/! (0, 0)) `shouldBe` "A"
      (wordsearch !/! (1, 7)) `shouldBe` "!voha4WP"
      (wordsearch !/! (7, 7)) `shouldBe` "*#"
      (wordsearch !/! (6, 7)) `shouldBe` "&@t"
      (wordsearch !/! (6, 1)) `shouldBe` "ung03VOH"
      (wordsearch !/! (6, 2)) `shouldBe` "!voha4WP"

  describe "htake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (htake wordsearch coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (htake wordsearch coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (htake wordsearch coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the width of the wordsearch" $ do
        it "returns the n characters to the right" $ do
          (htake wordsearch coord 4) `shouldBe` "TUVW"
          (htake wordsearch coord 3) `shouldBe` "TUV"
          (htake wordsearch coord 2) `shouldBe` "TU"
      context "and exceeds the width of the wordsearch" $ do
        it "returns the characters to the right up to the edge" $ do
          (htake wordsearch coord 100) `shouldBe` "TUVWX"
    context "when n is negative" $ do
      context "and doesn't exceed the width of the wordsearch" $ do
        it "returns the n characters to the left in reverse order" $ do
          (htake wordsearch coord (-4)) `shouldBe` "TSRQ"
          (htake wordsearch coord (-3)) `shouldBe` "TSR"
          (htake wordsearch coord (-2)) `shouldBe` "TS"
      context "and exceeds the width of the wordsearch" $ do
        it "returns the characters to the left in reverse order, up to the edge" $ do
          (htake wordsearch coord (-100)) `shouldBe` "TSRQ"

  
  describe "vtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (vtake wordsearch coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (vtake wordsearch coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (vtake wordsearch coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the height of the wordsearch" $ do
        it "returns the n characters below" $ do
          (vtake wordsearch coord 4) `shouldBe` "T20h"
          (vtake wordsearch coord 3) `shouldBe` "T20"
          (vtake wordsearch coord 2) `shouldBe` "T2"
      context "and exceeds the height of the wordsearch" $ do
        it "returns the characters below up to the edge" $ do
          (vtake wordsearch coord 100) `shouldBe` "T20hpx%"
    context "when n is negative" $ do
      context "and doesn't exceed the height of the wordsearch" $ do
        it "returns the n characters above in reverse order" $ do
          (vtake wordsearch coord (-3)) `shouldBe` "TLD"
          (vtake wordsearch coord (-2)) `shouldBe` "TL"
      context "and exceeds the height of the wordsearch" $ do
        it "returns the characters above in reverse order, up to the edge" $ do
          (vtake wordsearch coord (-100)) `shouldBe` "TLD"

  describe "ldtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (ldtake wordsearch coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (ldtake wordsearch coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (ldtake wordsearch coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters below" $ do
          (ldtake wordsearch coord 5) `shouldBe` "T18e"
          (ldtake wordsearch coord 3) `shouldBe` "T18"
          (ldtake wordsearch coord 2) `shouldBe` "T1"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters below up to the edge" $ do
          (ldtake wordsearch coord 100) `shouldBe` "T18e"
    context "when n is negative" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters above" $ do
          (ldtake wordsearch coord (-3)) `shouldBe` "TMF"
          (ldtake wordsearch coord (-2)) `shouldBe` "TM"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters above" $ do
          (ldtake wordsearch coord (-100)) `shouldBe` "TMF"

  describe "rdtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (rdtake wordsearch coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (rdtake wordsearch coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (rdtake wordsearch coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters below" $ do
          (rdtake wordsearch coord 5) `shouldBe` "T3bkt"
          (rdtake wordsearch coord 4) `shouldBe` "T3bk"
          (rdtake wordsearch coord 3) `shouldBe` "T3b"
          (rdtake wordsearch coord 2) `shouldBe` "T3"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters below up to the edge" $ do
          (rdtake wordsearch coord 100) `shouldBe` "T3bkt"
    context "when n is negative" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters above" $ do
          (rdtake wordsearch coord (-3)) `shouldBe` "TKB"
          (rdtake wordsearch coord (-2)) `shouldBe` "TK"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters above" $ do
          (rdtake wordsearch coord (-100)) `shouldBe` "TKB"
