{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CliForms (
    submitForm,
    Default (..),
) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as Bs
import Data.Char
import qualified Data.Yaml as Ya
import System.Directory
import System.Environment
import System.Process
import System.IO

class Default a where
    defaul :: a

submitForm :: (ToJSON a, FromJSON a, Default a) => String -> IO (Maybe a)
submitForm formName = do
    folder <- setupCacheDirectory
    showForm formName folder True

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
    Bool ->
    IO (Maybe a)
showForm formName folder isFirst = do
    let filePath = formPath formName folder False
    let filePathTemp = formPath formName folder True
    when isFirst $ createFile (defaul :: a) formName folder
    editFile formName folder
    fileString <- Bs.readFile filePathTemp
    let decodeE = Ya.decodeEither' fileString
    case decodeE of
        Left e -> showError formName folder e
        Right a -> do
            Bs.writeFile filePath fileString
            return a

showError ::
    forall a.
    (ToJSON a, FromJSON a, Default a) =>
    String ->
    FilePath ->
    Ya.ParseException ->
    IO (Maybe a)
showError formName folder exc = do
    putStrLn $ Ya.prettyPrintParseException exc
    hFlush stdout
    resp <- ask "Do you want to fix the file?"
    if resp then showForm formName folder False else return Nothing

ask :: String -> IO Bool
ask question = do
    putStr $ question ++ " y/n:"
    hFlush stdout
    line <- getLine
    case line of
        a | map toLower a == "n" -> return False
        a | map toLower a == "y" -> return True
        _ -> ask question

formPath :: String -> FilePath -> Bool -> FilePath
formPath formName folder temporary =
    folder
        ++ "/"
        ++ formName
        ++ (if temporary then ".temporary" else "")
        ++ ".yaml"

createFile :: (ToJSON a) => a -> String -> FilePath -> IO ()
createFile defaultData formName folder = do
    let filePath = formPath formName folder False
    let filePathTemp = formPath formName folder True
    fileExists <- doesFileExist filePath
    if fileExists then do
        let fileString = Ya.encode defaultData
        Bs.writeFile filePath fileString
        Bs.writeFile filePathTemp fileString
    else do
        fileString <- Bs.readFile filePath
        Bs.writeFile filePathTemp fileString

        
editFile :: String -> FilePath -> IO ()
editFile formName folder = do
    let filePath = formPath formName folder True
    editor <- getEnv "EDITOR"
    let editor' = if null editor then "vim" else editor
    callCommand $ editor' ++ " " ++ filePath
