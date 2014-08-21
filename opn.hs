{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import           Data.Ini (Ini(..), parseIni, lookupValue)
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
lookupBrowser = either error id . lookupValue "browser" "browser"

lookupCommand :: Ini -> Extension -> Maybe Command
lookupCommand (Ini ini) ext =
    case M.lookup "associations" ini of
        Nothing   -> error "Couldn't find required section 'associations'"
        Just cmds -> T.strip `fmap` M.lookup (T.drop 1 ext) cmds

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

readConfig :: FilePath -> IO Ini
readConfig home = do
    opnrcExists <- doesFileExist opnrc
    if opnrcExists
        then (either error id . parseIni . adjust) `fmap` T.readFile opnrc
        else error (opnrc ++ " does not exist (you need to create it)")
  where
    opnrc = home </> ".opnrc"
    adjust = T.unlines . map T.stripStart . T.lines

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
            conf <- readConfig =<< getHomeDirectory
            forM_ args $ \s ->
                getCommand conf s >>= \cmd -> run cmd [s]
