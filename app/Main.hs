{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import CliForms
import Data.Aeson
import GHC.Generics
import Data.Foldable

data DummyData
    = Emptyyy String
    | Data {data1 :: String, data2 :: Float, data3 :: [Int]}
    deriving (Generic, Show)
instance Default DummyData where
    defaul = Emptyyy "this is empty"
instance ToJSON DummyData
instance FromJSON DummyData

main :: IO ()
main = do
    ddM <- (submitForm "defaultForm" :: IO (Maybe DummyData))
    forM_ ddM print
