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
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Ini --(Ini(..), parseIni)
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
type Assoc     = HashMap Extension Command

lookupCommand :: Assoc -> Extension -> Maybe Command
lookupCommand assoc ext = T.strip `fmap` M.lookup (T.drop 1 ext) assoc

getMimeType :: FilePath -> IO MimeType
getMimeType fpath = fmap
    (T.pack . drop 1 . reverse . takeWhile (/= ':') . drop 1 . reverse)
    (readProcess "file" ["--mime-type", "-L", fpath] "")

getExtensions :: FilePath -> IO [Extension]
getExtensions fpath = do
    es <- guessExtensions `fmap` getMimeType fpath
    return $ case takeExtension fpath of { [] -> es; e -> T.pack e : es }

getCommand :: (Text, Assoc) -> PathOrURL -> IO Command
getCommand (browser, assoc) s = do
    fileExists <- doesFileExist s
    if fileExists
        then do
            exts <- getExtensions s
            case catMaybes (map (lookupCommand assoc) exts) of
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

trConfig :: Ini -> (Command, Assoc)
trConfig (Ini ini) = (browser, d)
  where
    d = M.fromList $ M.toList cmdDict >>= \(c,es) -> [(e,c) | e <- T.words es]
    browser  = let s = "browser" in maybe (errKey s) id $ M.lookup s (fndSec s)
    cmdDict  = fndSec "associations"
    fndSec s = maybe (errSec s) id $ M.lookup s ini
    errSec s = error $ "Couldn't find required section " ++ show s
    errKey s = error $ "Couldn't find required key "     ++ show s

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
            conf <- trConfig `fmap` (readConfig =<< getHomeDirectory)
            forM_ args $ \s ->
                getCommand conf s >>= \cmd -> run cmd [s]
