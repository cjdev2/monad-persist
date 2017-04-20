# monad-persist [![Build Status](https://travis-ci.org/cjdev/monad-persist.svg?branch=master)](https://travis-ci.org/cjdev/monad-persist)

This module provides an [mtl][]-style `MonadPersist` typeclass for [persistent][], as well as a `PersistT` monad transformer that implements it. This makes it easier to use persistent in an arbitrary monad transformer stack, rather than one that must be `ReaderT` over `MonadIO`.

```haskell
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Persist
import Data.Text (Text)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  email Text

  UniqueEmail email

  deriving Eq Show
|]


ghci> runNoLoggingT $ withSqliteConn ":memory:" $ \conn -> flip runSqlPersistT conn $ do
        runMigration migrateAll
        insert_ User { userName = "Alyssa", userEmail = "alyssa@example.com" }
        users <- selectList [] []
        return (users :: [Entity User])

Migrating: CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"email" VARCHAR NOT NULL,CONSTRAINT "unique_email" UNIQUE ("email"))
[Entity {entityKey = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = User {userName = "Alyssa", userEmail = "alyssa@example.com"}}]
```

[mtl]: https://hackage.haskell.org/package/mtl
[persistent]: https://hackage.haskell.org/package/persistent
