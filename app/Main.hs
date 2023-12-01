{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Lib
import GHC.Generics
import Data.Aeson

data DummyData = Emptyyy String | Data { data1 :: String, data2 :: Float, data3 :: [Int] } deriving (Generic, Show)
instance Default DummyData where
  defaul = Emptyyy "this is empty"
instance ToJSON DummyData where
instance FromJSON DummyData where

main :: IO ()
main = do
  ddM <- (submitForm "defaultForm" :: IO (Maybe DummyData))
  case ddM of
    Nothing -> return ()
    Just dd -> print dd
