{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import           Data.Ini (Ini(..), readIniFile, lookupValue)
import           Control.Monad (forM_)
import           System.Process (runProcess, readProcess)
import           System.Environment (getArgs)
import           System.FilePath (takeExtension, (</>))
import           System.Directory (doesFileExist, getHomeDirectory)
import           Network.URI (isAbsoluteURI)
import           Opn.MimeType (guessExtensions)

type Command   = Text
type PathOrURL = String
type MimeType  = Text
type Extension = Text

lookupBrowser :: Ini -> Command
lookupBrowser = either error id . lookupValue "BROWSER" "browser"

lookupCommand :: Ini -> Extension -> Maybe Command
lookupCommand (Ini ini) ext =
    case M.lookup "ASSOCIATIONS" ini of
        Nothing   -> error "Couldn't find required section 'ASSOCIATIONS'"
        Just cmds -> M.lookup (T.drop 1 ext) cmds

getMimeType :: FilePath -> IO MimeType
getMimeType fpath = fmap
    (T.pack . drop 1 . reverse . takeWhile (/= ':') . drop 1 . reverse)
    (readProcess "file" ["--mime-type", "-L", fpath] "")

getExtensions :: FilePath -> IO [Extension]
getExtensions fpath =
    case takeExtension fpath of
        []  -> guessExtensions `fmap` getMimeType fpath
        ext -> return [T.pack ext]

getCommand :: Ini -> PathOrURL -> IO Command
getCommand conf s = do
    let browser = lookupBrowser conf
    fileExists <- doesFileExist s
    if fileExists
        then do
            exts <- getExtensions s
            case catMaybes (map (lookupCommand conf) exts) of
                []      -> return browser
                (cmd:_) -> return cmd
        else
            if isAbsoluteURI s
                then return browser
                else error (show s ++
                       " is neither a file-path nor an absolute URL")

run :: Command -> [String] -> IO ()
run cmd args =
    runProcess (T.unpack cmd) args Nothing Nothing Nothing Nothing Nothing >>
    return ()

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "usage: opn (file | url)..."
        else do
            home <- getHomeDirectory
            conf <- either error id `fmap` readIniFile (home </> ".opnrc")
            forM_ args $ \s -> getCommand conf s >>= \cmd -> run cmd [s]
