{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow ((&&&))
import qualified Data.Aeson.Extended as Aeson
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.ByteString as Byte
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Proxy (Proxy(..))
import qualified Data.String as String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import qualified GitHub
import qualified GitHub.Endpoints.Repos as Github
import qualified Path
import qualified Stack.Types.Config as Stack
import qualified System.Directory as Dir
import qualified System.Environment as Env

main :: IO ()
main = do
  auth <- getAuth
  branches <- getBranchShaMap auth "scott-fleischman" "greek-grammar"
  mapM_ print $ HashMap.toList branches -- GitHub.branchCommitSha . GitHub.branchCommit

  project <- getProject
  print project
  Byte.putStr $ Yaml.encode project

  url <- getUrl "git@github.com:commercialhaskell/stack.git"
  print url

getUrl :: Text -> IO (Github.Name GitHub.Owner, Github.Name Github.Repo)
getUrl url =
  case Parsec.parse
    (do
      _ <- Parsec.string "git@github.com:"
      owner <- Parsec.takeWhile (/= '/')
      _ <- Parsec.char '/'
      repo <- Text.pack <$> Parsec.manyTill Parsec.anyChar (Parsec.string ".git")
      return (GitHub.mkName Proxy owner, GitHub.mkName Proxy repo)
    )
    url
  of
  Parsec.Done _ r -> return r
  f -> fail . Text.unpack $ Text.concat ["Unable to parse url: ", url, " -- ", Text.pack (show f)]

getProject :: IO Stack.Project
getProject = do
  stackFile <- getStackFile
  eglobal <- Yaml.decodeFileEither $ Path.toFilePath stackFile
  case eglobal of
    Left decodeError -> fail $ "Error decoding global stack.yaml: " ++ show decodeError
    Right val -> case Yaml.parseEither (Stack.parseProjectAndConfigMonoid $ Path.parent stackFile) val of
      Left parseError -> fail $ "Error parsing global stack.yaml: " ++ parseError
      Right (Aeson.WithJSONWarnings (Stack.ProjectAndConfigMonoid project _) _) -> return project

getBranchShaMap :: Maybe GitHub.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> IO (HashMap Text Text)
getBranchShaMap auth owner repo = do
  possibleBranches <- Github.branchesFor' auth owner repo
  case possibleBranches of
    Left error -> fail $ "Error getting branches for " ++ nameShow owner ++ "/" ++ nameShow repo ++ "\n" ++ show error
    Right branches -> return . HashMap.fromList . Vector.toList . fmap (Github.branchName &&& GitHub.branchCommitSha . GitHub.branchCommit) $ branches

getAuth :: IO (Maybe GitHub.Auth)
getAuth = do
  token <- Env.lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . String.fromString <$> token)

nameShow :: GitHub.Name a -> String
nameShow = Text.unpack . Github.untagName

getStackFile :: IO (Path.Path Path.Abs Path.File)
getStackFile = do
  let defaultStackFile = "stack.yaml"
  args <- Env.getArgs
  case args of
    [] -> do
      currentDirString <- Dir.getCurrentDirectory
      currentDir <- Path.parseAbsDir currentDirString
      file <- Path.parseRelFile defaultStackFile
      return $ currentDir Path.</> file
    path : _ -> do
      normalized <- Dir.canonicalizePath path
      isFile <- Dir.doesFileExist normalized
      if isFile
        then Path.parseAbsFile normalized
        else do
          dir <- Path.parseAbsDir normalized
          file <- Path.parseRelFile defaultStackFile
          return $ dir Path.</> file
