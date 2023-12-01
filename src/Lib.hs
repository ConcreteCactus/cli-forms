{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
    submitForm,
    Default(..),
) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as Bs
import Data.Char
import qualified Data.Yaml as Ya
import System.Directory
import System.Environment
import System.Process

class Default a where
    defaul :: a

submitForm :: (ToJSON a, FromJSON a, Default a) => String -> IO (Maybe a)
submitForm formName = do
    folder <- setupCacheDirectory
    showForm formName folder

setupCacheDirectory :: IO FilePath
setupCacheDirectory = do
    homeFolder <- getEnv "HOME"
    let cacheFolder = homeFolder ++ "/.cache/cli-forms"
    dirExists <- doesDirectoryExist cacheFolder
    unless dirExists $ createDirectory cacheFolder
    return cacheFolder

showForm ::
    forall a.
    (ToJSON a, FromJSON a, Default a) =>
    String ->
    FilePath ->
    IO (Maybe a)
showForm formName folder = do
    let filePath = formPath formName folder
    _ <- createFile formName folder :: IO a
    editFile formName folder
    decodeE <- Ya.decodeFileEither filePath
    case decodeE of
        Left e -> showError formName folder e
        Right a -> return a

showError ::
    forall a.
    (ToJSON a, FromJSON a, Default a) =>
    String ->
    FilePath ->
    Ya.ParseException ->
    IO (Maybe a)
showError formName folder exc = do
    putStrLn $ show exc
    resp <- ask "Do you want to fix the file?"
    if resp then showForm formName folder else return Nothing

ask :: String -> IO Bool
ask question = do
    putStr $ question ++ " y/n:"
    line <- getLine
    case toLower (head line) of
        'n' -> return False
        'y' -> return True
        _ -> ask question

formPath :: String -> FilePath -> FilePath
formPath formName folder = folder ++ "/" ++ formName ++ ".yaml"

createFile :: (ToJSON a, Default a) => String -> FilePath -> IO a
createFile formName folder = do
    let filePath = formPath formName folder
    fileExists <- doesFileExist filePath
    let defaultData = defaul
    unless fileExists $ do
        let fileString = Ya.encode defaultData
        Bs.writeFile filePath fileString
    return defaultData

editFile :: String -> FilePath -> IO ()
editFile formName folder = do
    let filePath = formPath formName folder
    editor <- getEnv "EDITOR"
    let editor' = if null editor then "vim" else editor
    callCommand $ editor' ++ " " ++ filePath
