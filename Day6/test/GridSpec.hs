module Main where

import Test.Hspec
import Grid

main :: IO ()
main = hspec $ do
  let grid = ["ABCDEFGH"
                   ,"IJKLMNOP"
                   ,"QRSTUVWX"
                   ,"YZ123456"
                   ,"7890abcd"
                   ,"efghijkl"
                   ,"mnopqrst"
                   ,"uvwxyz@#"
                   ,"!£$%^&*("] :: Grid Char
  let coord = (3, 2) -- 'T'

  describe "width" $ do
    it "returns the width" $ do
      (width grid) `shouldBe` 8

  describe "height" $ do
    it "returns the height" $ do
      (height grid) `shouldBe` 9

  describe "!.!" $ do
    it "returns the character at coord" $ do
      (grid !.! coord) `shouldBe` 'T'

  describe "!-!" $ do
    it "returns the row" $ do
      (grid !-! 1) `shouldBe` "IJKLMNOP"

  describe "!|!" $ do
    it "returns the column" $ do
      (grid !|! 1) `shouldBe` "BJRZ8fnv£"

  describe "!\\!" $ do
    it "returns the diagonal" $ do
      (grid !\! coord) `shouldBe` "BKT3bkt"
      (grid !\! (width grid - 1, 0)) `shouldBe` "H"
      (grid !\! (0, height grid -1)) `shouldBe` "!"
      (grid !\! (1, 7)) `shouldBe` "mv$"
      (grid !\! (7, 7)) `shouldBe` "AJS2ajs#"
      (grid !\! (6, 7)) `shouldBe` "IR10ir@("
      (grid !\! (6, 1)) `shouldBe` "FOX"
      (grid !\! (6, 2)) `shouldBe` "ENW6"

  describe "!/!" $ do
    it "returns the diagonal" $ do
      (grid !/! coord) `shouldBe` "e81TMF"
      (grid !/! (width grid - 1, height grid -1)) `shouldBe` "("
      (grid !/! (0, 0)) `shouldBe` "A"
      (grid !/! (1, 7)) `shouldBe` "!voha4WP"
      (grid !/! (7, 7)) `shouldBe` "*#"
      (grid !/! (6, 7)) `shouldBe` "&@t"
      (grid !/! (6, 1)) `shouldBe` "ung03VOH"
      (grid !/! (6, 2)) `shouldBe` "!voha4WP"

  describe "htake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (htake grid coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (htake grid coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (htake grid coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the width of the grid" $ do
        it "returns the n characters to the right" $ do
          (htake grid coord 4) `shouldBe` "TUVW"
          (htake grid coord 3) `shouldBe` "TUV"
          (htake grid coord 2) `shouldBe` "TU"
      context "and exceeds the width of the grid" $ do
        it "returns the characters to the right up to the edge" $ do
          (htake grid coord 100) `shouldBe` "TUVWX"
    context "when n is negative" $ do
      context "and doesn't exceed the width of the grid" $ do
        it "returns the n characters to the left in reverse order" $ do
          (htake grid coord (-4)) `shouldBe` "TSRQ"
          (htake grid coord (-3)) `shouldBe` "TSR"
          (htake grid coord (-2)) `shouldBe` "TS"
      context "and exceeds the width of the grid" $ do
        it "returns the characters to the left in reverse order, up to the edge" $ do
          (htake grid coord (-100)) `shouldBe` "TSRQ"

  
  describe "vtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (vtake grid coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (vtake grid coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (vtake grid coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the height of the grid" $ do
        it "returns the n characters below" $ do
          (vtake grid coord 4) `shouldBe` "T20h"
          (vtake grid coord 3) `shouldBe` "T20"
          (vtake grid coord 2) `shouldBe` "T2"
      context "and exceeds the height of the grid" $ do
        it "returns the characters below up to the edge" $ do
          (vtake grid coord 100) `shouldBe` "T20hpx%"
    context "when n is negative" $ do
      context "and doesn't exceed the height of the grid" $ do
        it "returns the n characters above in reverse order" $ do
          (vtake grid coord (-3)) `shouldBe` "TLD"
          (vtake grid coord (-2)) `shouldBe` "TL"
      context "and exceeds the height of the grid" $ do
        it "returns the characters above in reverse order, up to the edge" $ do
          (vtake grid coord (-100)) `shouldBe` "TLD"

  describe "ldtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (ldtake grid coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (ldtake grid coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (ldtake grid coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters below" $ do
          (ldtake grid coord 5) `shouldBe` "T18e"
          (ldtake grid coord 3) `shouldBe` "T18"
          (ldtake grid coord 2) `shouldBe` "T1"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters below up to the edge" $ do
          (ldtake grid coord 100) `shouldBe` "T18e"
    context "when n is negative" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters above" $ do
          (ldtake grid coord (-3)) `shouldBe` "TMF"
          (ldtake grid coord (-2)) `shouldBe` "TM"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters above" $ do
          (ldtake grid coord (-100)) `shouldBe` "TMF"

  describe "rdtake" $ do
    context "when n is 0" $ do
      it "returns an empty string" $ do
        (rdtake grid coord 0) `shouldBe` ""
    context "when n is 1" $ do
      it "returns the character at coord" $ do
        (rdtake grid coord 1) `shouldBe` "T"
    context "when n is -1" $ do
      it "returns the character at coord" $ do
        (rdtake grid coord (-1)) `shouldBe` "T"
    context "when n is positive" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters below" $ do
          (rdtake grid coord 5) `shouldBe` "T3bkt"
          (rdtake grid coord 4) `shouldBe` "T3bk"
          (rdtake grid coord 3) `shouldBe` "T3b"
          (rdtake grid coord 2) `shouldBe` "T3"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters below up to the edge" $ do
          (rdtake grid coord 100) `shouldBe` "T3bkt"
    context "when n is negative" $ do
      context "and doesn't exceed the length of the diagonal" $ do
        it "returns the n characters above" $ do
          (rdtake grid coord (-3)) `shouldBe` "TKB"
          (rdtake grid coord (-2)) `shouldBe` "TK"
      context "and exceeds the length of the diagonal" $ do
        it "returns the characters above" $ do
          (rdtake grid coord (-100)) `shouldBe` "TKB"

  describe "set" $ do
    context "when coord exists in the grid" $ do
      it "updates the character at the coord" $ do
        let new = set grid coord '~'
        new !.! coord `shouldBe` '~'
        height new `shouldBe` height grid
        width new `shouldBe` width grid
        length (new !-! snd coord) `shouldBe` length (grid !-! snd coord)
        take (fst coord) (new !-! snd coord) ++ drop (fst coord + 1) (new !-! snd coord) `shouldBe` take (fst coord) (grid !-! snd coord) ++ drop (fst coord + 1) (grid !-! snd coord)
    context "when coord does not exist in the grid" $ do
      it "leaves the grid unchaged" $ do
        let new = set grid (1000000, 1000000) 'X'
        new `shouldBe` grid
        let new' = set grid (-100000, -100000) 'X'
        new `shouldBe` grid
