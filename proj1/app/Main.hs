{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader
import Control.Monad.State
import System.Directory

walkDirectory ::
  ( MonadState [FilePath] m
  , MonadIO m)
  => m ()
walkDirectory = do
  paths <- get
  case paths of
    [] -> pure ()
    path : rest -> do
      liftIO $ putStrLn path
      put rest
      isDir <- liftIO $ doesDirectoryExist path
      when isDir $ do
        ds <- liftIO $ listDirectory path
        put $ rest ++ map ((path ++ "/") ++) ds
      walkDirectory


main :: IO ()
main = do
  ds <- runReaderT (execStateT walkDirectory ["."]) 3
  forM_ ds print
