{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson.Extended as Aeson
import qualified Data.ByteString as Byte
import qualified Data.String as String
import qualified Data.Text as Text
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
  possibleBranches <- Github.branchesFor' auth "scott-fleischman" "greek-grammar"
  case possibleBranches of
    Left error -> putStrLn $ "Error: " ++ show error
    Right branches -> do
      mapM_ (print . GitHub.branchCommitSha . GitHub.branchCommit) branches

      stackFile <- getStackFile
      eglobal <- Yaml.decodeFileEither $ Path.toFilePath stackFile
      case eglobal of
        Left decodeError -> fail $ "Error decoding global stack.yaml: " ++ show decodeError
        Right val -> case Yaml.parseEither (Stack.parseProjectAndConfigMonoid $ Path.parent stackFile) val of
          Left parseError -> fail $ "Error parsing global stack.yaml: " ++ parseError
          Right (Aeson.WithJSONWarnings (Stack.ProjectAndConfigMonoid project _) _) ->
            Byte.putStr $ Yaml.encode project

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
  token <- Env.lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . String.fromString <$> token)

getStackFile :: IO (Path.Path Path.Abs Path.File)
getStackFile = do
  args <- Env.getArgs
  case args of
    [] -> do
      currentDirString <- Dir.getCurrentDirectory
      currentDir <- Path.parseAbsDir currentDirString
      file <- Path.parseRelFile "stack.yaml"
      return $ currentDir Path.</> file
    path : _ -> Path.parseAbsFile path
 