{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Ini (Ini(..), parseIni)
import           Options.Applicative
import           Control.Monad (forM_, void, unless)
import           System.IO (hPutStrLn, stderr)
import           System.Process (runProcess, readProcess)
import           System.FilePath (takeExtension, (</>))
import           System.Directory (doesFileExist, getHomeDirectory)
import           Network.URI (isAbsoluteURI)
import           Opn.Guess (guess)

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
name = "opn 0.1.3"

optsParser :: Parser (Maybe Opts)
optsParser = hiddenHelp <*> versionOpt <|> (Just <$> (Opts
    <$> switch (long "dry-run" <> dryrunHelp)
    <*> some (argument str (metavar "PATHS..."))))
  where
    dryrunHelp = help "Display command(s) that would be executed, then exit."
    hiddenHelp = abortOption ShowHelpText $ hidden <> short 'h' <> long "help"
    versionOpt = flag' Nothing (long "version")

lookupCommand :: ECMap -> Extension -> Maybe Command
lookupCommand m ext = T.strip <$> M.lookup ext m

getExtensions :: FilePath -> IO [Extension]
getExtensions fpath =
    (ext ++) <$> guess <$> mimeType
  where
    ext = case takeExtension fpath of { [] -> []; e -> [T.pack e]}
    mimeType = getValue <$> runFileCmd
    getValue = T.pack . drop 1 . reverse . takeWhile (/= ':') . drop 1 . reverse
    runFileCmd = readProcess "file" ["--mime-type", "-L", fpath] ""

getCommand :: Config -> PathOrURL -> IO (Maybe Command)
getCommand (browser, m) s = doesFileExist s >>= getCmd
  where
    getCmd True = do
        exts <- getExtensions s
        case mapMaybe (lookupCommand m) exts of
            []      -> return (Just browser)
            (cmd:_) -> return (Just cmd)
    getCmd False =
        if isAbsoluteURI s
            then return (Just browser)
            else do
                hPutStrLn stderr (show s ++
                    " is neither a file-path nor an absolute URL; skipping.")
                return Nothing

readIni :: Homedir -> IO Ini
readIni home = do
    opnconfigExists <- doesFileExist opnconfig
    unless opnconfigExists $ do
        hPutStrLn stderr (opnconfig ++ " does not exist; creating a stub.")
        T.writeFile opnconfig
            "[browser]\nbrowser: chromium\n\n[associations]\nchromium: html\n"
    (either error id . parseIni . adjust) <$> T.readFile opnconfig
  where
    opnconfig = home </> ".opnconfig"
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
readConfig = fmap mkConfig . readIni

run :: Command -> [String] -> IO ()
run cmd args = void $
    runProcess (T.unpack cmd) args Nothing Nothing Nothing Nothing Nothing

opn :: Maybe Opts -> IO ()
opn Nothing     = putStrLn name
opn (Just opts) = do
    conf <- readConfig =<< getHomeDirectory
    forM_ (paths opts) $ \path ->
        getCommand conf path >>= \c ->
            case c of
                Just cmd -> run' cmd [path]
                Nothing  -> return ()
  where
    run' = if dryrun opts then \c [s] -> T.putStr c >> putStrLn (' ':s) else run

main :: IO ()
main =
    customExecParser (prefs showHelpOnError) (info optsParser fullDesc) >>= opn
