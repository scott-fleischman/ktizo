{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.String as String
import qualified Data.Text as Text
import qualified GitHub
import qualified GitHub.Endpoints.Repos as Github
import qualified System.Environment as Env

main :: IO ()
main = do
  auth <- getAuth
  possibleBranches <- Github.branchesFor' auth "scott-fleischman" "greek-grammar"
  case possibleBranches of
    Left error -> putStrLn $ "Error: " ++ show error
    Right branches -> mapM_ (print . GitHub.branchCommitSha . GitHub.branchCommit) branches

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
    token <- Env.lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . String.fromString <$> token)
