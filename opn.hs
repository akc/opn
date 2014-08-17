{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, unpack)
import Data.Ini (Ini, readIniFile, lookupValue)
import Control.Monad (forM_)
import System.Process (runProcess)
import System.Environment (getArgs)
import System.FilePath (hasExtension, takeExtension, (</>))
import System.Directory (doesFileExist, getHomeDirectory)

getAction :: Ini -> String -> IO String
getAction conf s =
    (f . ( && hasExtension s)) `fmap` doesFileExist s
  where
    ext = pack . drop 1 $ takeExtension s
    browser  = unpackOr error $ lookupValue "BROWSER" "browser" conf
    f True   = unpackOr (const browser) $ lookupValue "ASSOCIATIONS" ext conf
    f False  = browser
    unpackOr = flip either unpack

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "usage: opn (file | url)..."
        else do
            home <- getHomeDirectory
            conf <- either error id `fmap` readIniFile (home </> ".opnrc")
            forM_ args $ \s -> do
                a <- getAction conf s
                _ <- runProcess a [s] Nothing Nothing Nothing Nothing Nothing
                return ()
