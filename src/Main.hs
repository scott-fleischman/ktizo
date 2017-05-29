{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson.Extended as Aeson
import qualified Data.ByteString as Byte
import qualified Data.String as String
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
  mapM_ print branches -- GitHub.branchCommitSha . GitHub.branchCommit

  stackFile <- getStackFile
  eglobal <- Yaml.decodeFileEither $ Path.toFilePath stackFile
  case eglobal of
    Left decodeError -> fail $ "Error decoding global stack.yaml: " ++ show decodeError
    Right val -> case Yaml.parseEither (Stack.parseProjectAndConfigMonoid $ Path.parent stackFile) val of
      Left parseError -> fail $ "Error parsing global stack.yaml: " ++ parseError
      Right (Aeson.WithJSONWarnings (Stack.ProjectAndConfigMonoid project _) _) ->
        Byte.putStr $ Yaml.encode project

getBranchShaMap :: Maybe GitHub.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> IO (Vector Github.Branch)
getBranchShaMap auth owner repo = do
  possibleBranches <- Github.branchesFor' auth owner repo
  case possibleBranches of
    Left error -> do
      putStrLn $ "Error: " ++ show error
      return Vector.empty
    Right branches -> return branches
--      mapM_ print branches -- GitHub.branchCommitSha . GitHub.branchCommit

getAuth :: IO (Maybe GitHub.Auth)
getAuth = do
  token <- Env.lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . String.fromString <$> token)

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
