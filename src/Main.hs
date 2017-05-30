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
  project <- getProject
  let packages = Stack.projectPackages project
  newPackages <- mapM (updatePackageEntry $ updatePackageLocation auth) packages
  let newProject = project { Stack.projectPackages = newPackages }
  Byte.putStr $ Yaml.encode newProject

updatePackageEntry :: (Stack.PackageLocation -> IO Stack.PackageLocation) -> Stack.PackageEntry -> IO Stack.PackageEntry
updatePackageEntry f entry = do
  location <- f $ Stack.peLocation entry
  return entry { Stack.peLocation = location }

updatePackageLocation :: Maybe Github.Auth -> Stack.PackageLocation -> IO Stack.PackageLocation
updatePackageLocation auth orig@(Stack.PLRemote url (Stack.RPTGit sha)) = do
  (owner, repo) <- urlToOwnerRepo url
  putStrLn $ "Getting branches for " ++ nameShow owner ++ "/" ++ nameShow repo
  branchMap <- getBranchShaMap auth owner repo
  case HashMap.lookup "master" branchMap of
    Just newSha ->
      if sha == newSha
      then return orig
      else do
        putStrLn $ "  updating to commit " ++ Text.unpack newSha
        return $ Stack.PLRemote url (Stack.RPTGit newSha)
    Nothing -> fail $ "Unable to find master branch for " ++ Text.unpack url
updatePackageLocation _ location = return location

urlToOwnerRepo :: Text -> IO (Github.Name GitHub.Owner, Github.Name Github.Repo)
urlToOwnerRepo url =
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
