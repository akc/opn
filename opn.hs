{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.Maybe (catMaybes)
import           Data.Functor ((<$>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Ini (Ini(..), parseIni)
import           Control.Monad (forM_)
import           System.Process (runProcess, readProcess)
import           System.Environment (getArgs)
import           System.FilePath (takeExtension, (</>))
import           System.Directory (doesFileExist, getHomeDirectory)
import           Network.URI (isAbsoluteURI)
import           Opn.MimeType (guessExtensions)

type Command   = Text
type Browser   = Command
type PathOrURL = String
type Homedir   = String
type Extension = Text
type ECMap     = HashMap Extension Command
type Config    = (Browser, ECMap)

lookupCommand :: ECMap -> Extension -> Maybe Command
lookupCommand m ext = T.strip <$> M.lookup (T.drop 1 ext) m

getExtensions :: FilePath -> IO [Extension]
getExtensions fpath =
    (ext ++) <$> guessExtensions <$> mimeType
  where
    ext = case takeExtension fpath of { [] -> []; e -> [T.pack e]}
    mimeType = getValue <$> runFileCmd
    getValue = T.pack . drop 1 . reverse . takeWhile (/= ':') . drop 1 . reverse
    runFileCmd = readProcess "file" ["--mime-type", "-L", fpath] ""

getCommand :: Config -> PathOrURL -> IO Command
getCommand (browser, m) s = do
    fileExists <- doesFileExist s
    if fileExists
        then do
            exts <- getExtensions s
            case catMaybes (map (lookupCommand m) exts) of
                []      -> return browser
                (cmd:_) -> return cmd
        else
            if isAbsoluteURI s
                then return browser
                else error (show s ++
                       " is neither a file-path nor an absolute URL")

read_opnrc :: Homedir -> IO Ini
read_opnrc home = do
    opnrcExists <- doesFileExist opnrc
    if opnrcExists
        then (either error id . parseIni . adjust) <$> T.readFile opnrc
        else error (opnrc ++ " does not exist (you need to create it)")
  where
    opnrc = home </> ".opnrc"
    adjust = T.unlines . map T.stripStart . T.lines

mkConfig :: Ini -> Config
mkConfig (Ini ini) = (browser, d)
  where
    d = M.fromList $ M.toList cmdMap >>= \(c,es) -> [(e,c) | e <- T.words es]
    browser  = let s = "browser" in maybe (errKey s) id $ M.lookup s (getSec s)
    cmdMap   = getSec "associations"
    getSec s = maybe (errSec s) id $ M.lookup s ini
    errSec s = error $ "Couldn't find required section " ++ show s
    errKey s = error $ "Couldn't find required key "     ++ show s

readConfig :: Homedir -> IO Config
readConfig = fmap mkConfig . read_opnrc

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
