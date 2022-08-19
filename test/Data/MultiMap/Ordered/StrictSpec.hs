{-# LANGUAGE TypeApplications #-}

module Data.MultiMap.Ordered.StrictSpec where

import Test.Hspec
import qualified Data.Map.Ordered as MOM
import qualified Data.MultiMap.Ordered.Strict as OM

spec :: Spec
spec = do
  describe "inserting elements" $ do
    it "one by one" $ do
      let theMap = foldr (uncurry OM.insert) mempty [(1, 3), (1, 2), (1, 1), (3, 3), (3, 2), (2, 1), (2, 3), (2, 2), (3, 1)]
      theMap == MOM.fromList @Int @[Int] [(3, [1, 2, 3]), (2, [2, 3, 1]), (1, [1, 2, 3])]

  it "whole list" $ do
    let theMap = OM.insert 1 3 $ OM.insert 1 2 $ OM.insert 1 1
          $ OM.insertAll 3 [2, 3] $ OM.insertAll 2 [2, 3] $ OM.insert 3 1 $ OM.insert 2 1 mempty
    theMap == MOM.fromList @Int @[Int] [(2, [1, 2, 3]), (3, [1, 2, 3]), (1, [1, 2, 3])]

  describe "conversion" $ do
    it "from list" $ do
      let theMap = OM.fromList [(2, 1), (3, 1), (2, 2), (1, 1), (2, 3), (1, 2), (3, 2), (3, 3), (1, 3)]
      theMap == MOM.fromList @Int @[Int] [(2, [1, 2, 3]), (3, [1, 2, 3]), (1, [1, 2, 3])]

    it "to list" $ do
      let theList = OM.toList $ MOM.fromList @Int @[Int] [(2, [1, 2, 3]), (3, [1, 2, 3]), (1, [1, 2, 3])]
      theList == [(2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3), (1, 1), (1, 2), (1, 3)]
