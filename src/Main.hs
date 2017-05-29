{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.String as String
import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub
import qualified System.Environment as Env

main :: IO ()
main = do
  auth <- getAuth
  user <- GitHub.userInfoFor' auth "scott-fleischman"
  print user

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
    token <- Env.lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . String.fromString <$> token)
