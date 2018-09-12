{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.PersistSpec (spec) where

import qualified Test.Hspec as Hspec

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
import Test.Hspec hiding (shouldBe)

import Control.Monad.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  email Text

  UniqueEmail email

  deriving Eq Show
|]

runSqlite :: (MonadIO m, MonadBaseControl IO m, MonadUnliftIO m) => SqlPersistT (NoLoggingT m) a -> m a
runSqlite x = runNoLoggingT $ withSqliteConn ":memory:" (runSqlPersistT (runMigrationSilent migrateAll >> x))

shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe x y = liftIO $ Hspec.shouldBe x y

spec :: Spec
spec = describe "MonadPersist" $ do
  it "can make queries against a database" $ example $ do
    result :: [Entity User] <- runSqlite $ selectList [] []
    result `shouldBe` []

  it "can insert records into a database" $ example $ runSqlite $ do
    let user = User { userName = "Alyssa", userEmail = "alyssa@example.com" }
    key <- insert user
    result <- get key
    result `shouldBe` Just user

  it "can query records by unique keys" $ example $ runSqlite $ do
    let user = User { userName = "Alyssa", userEmail = "alyssa@example.com" }
    insert_ user
    result <- getBy $ UniqueEmail "alyssa@example.com"
    fmap entityVal result `shouldBe` Just user
