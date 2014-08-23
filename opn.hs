{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Ini (Ini(..), parseIni)
import           Options.Applicative
import           Control.Monad (forM_, void)
import           System.Process (runProcess, readProcess)
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

data Opts = Opts
    { dryrun  :: Bool
    , paths   :: [String]
    }

name :: String
name = "opn 0.1.0"

optsParser :: Parser (Maybe Opts)
optsParser = hiddenHelp <*> versionParser <|> (Just <$> (Opts
    <$> switch (long "dry-run" <> dryHelp)
    <*> some (argument str (metavar "PATHS..."))))
  where
    dryHelp = help "Display command(s) that would be executed, then exit." )
    hiddenHelp = abortOption ShowHelpText $ hidden <> short 'h' <> long "help"
    versionParser = flag' Nothing (long "version")

lookupCommand :: ECMap -> Extension -> Maybe Command
lookupCommand m ext = T.strip <$> M.lookup ext m

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
            case mapMaybe (lookupCommand m) exts of
                []      -> return browser
                (cmd:_) -> return cmd
        else
            if isAbsoluteURI s
                then return browser
                else error (show s ++
                       " is neither a file-path nor an absolute URL")

readOpnrc :: Homedir -> IO Ini
readOpnrc home = do
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
    browser  = let s = "browser" in fromMaybe (errKey s) $ M.lookup s (getSec s)
    cmdMap   = getSec "associations"
    getSec s = fromMaybe (errSec s) $ M.lookup s ini
    errSec s = error $ "Couldn't find required section " ++ show s
    errKey s = error $ "Couldn't find required key "     ++ show s

readConfig :: Homedir -> IO Config
readConfig = fmap mkConfig . readOpnrc

run :: Command -> [String] -> IO ()
run cmd args = void $
    runProcess (T.unpack cmd) args Nothing Nothing Nothing Nothing Nothing

opn :: Maybe Opts -> IO ()
opn Nothing     = putStrLn name
opn (Just opts) = do
    conf <- readConfig =<< getHomeDirectory
    forM_ (paths opts) $ \path ->
        getCommand conf path >>= \cmd -> run' cmd [path]
  where
    run' = if dryrun opts then \c [s] -> T.putStr c >> putStrLn (' ':s) else run

main :: IO ()
main =
    customExecParser (prefs showHelpOnError) (info optsParser fullDesc) >>= opn
