{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides an @mtl@-style 'MonadPersist' typeclass for @persistent@,
  as well as a 'PersistT' monad transformer that implements it. If you use this
  module, it is recommended that you do /not/ import "Database.Persist.Class" or
  "Database.Persist.Sql", since this module defines its own versions of the
  things defined in those modules and exports them.

  Most notably, the 'SqlPersistT' transformer defined in this module is distinct
  from the 'Database.Persist.Sql.SqlPersistT' defined in "Database.Persist.Sql".
  The former is an alias for the 'PersistT' type defined in this module, while
  the latter is an alias for 'ReaderT' over a 'SqlBackend'.

  In general, if you are using a SQL database, you should use the helper aliases
  'MonadSqlPersist' and 'SqlPersistT' to avoid ambiguous types and complicated
  type errors.
-}
module Control.Monad.Persist
  ( module Database.Persist.Sql
  -- * MonadPersist
  , MonadPersist(..)
  , PersistT
  , runPersistT
  -- * Specialized to SqlBackend
  , MonadSqlPersist
  , SqlPersistT
  , runSqlPersistT
  ) where

import qualified Database.Persist.Sql as Sql

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, LoggingT, NoLoggingT)
import Control.Monad.Reader (MonadReader(..), ReaderT, mapReaderT, runReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Control
  ( ComposeSt
  , MonadBaseControl(..)
  , MonadTransControl(..)
  , defaultLiftBaseWith
  , defaultLiftWith
  , defaultRestoreM
  , defaultRestoreT
  )
import Control.Monad.Writer (MonadWriter, WriterT)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql
  ( CautiousMigration
  , DeleteCascade
  , Entity
  , Filter
  , IsPersistBackend
  , Key
  , Migration
  , PersistQueryRead
  , PersistQueryWrite
  , PersistRecordBackend
  , PersistStoreRead
  , PersistStoreWrite
  , PersistUniqueRead
  , PersistUniqueWrite
  , PersistValue
  , RawSql
  , SelectOpt
  , Sql
  , SqlBackend
  , Unique
  , Update
  , runSqlConn
  )

-- | A concrete monad transformer that implements 'MonadPersist'. To run it, use
-- 'runPersistT'.
--
-- If you are using a SQL database, it’s /highly/ recommended that you use the
-- simpler 'SqlPersistT' alias, instead.
newtype PersistT backend m a = PersistT (ReaderT backend m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadBase b, MonadError e
           , MonadIO, MonadLogger, MonadLoggerIO, MonadState s, MonadWriter w )

-- | Runs a 'PersistT' computation with an arbitrary backend.
--
-- If you are using a SQL database, it’s /highly/ recommended that you use
-- 'runSqlPersistT', instead, which will run the computation in a transaction.
runPersistT :: IsPersistBackend backend => PersistT backend m a -> backend -> m a
runPersistT (PersistT m) = runReaderT m
{-# INLINE runPersistT #-}

-- | A simple alias that specializes 'PersistT' to 'SqlBackend'.
type SqlPersistT = PersistT SqlBackend

-- | Runs a 'SqlPersistT' computation against a SQL database. __Unlike__
-- 'runPersistT', the computation is run inside a transaction.
runSqlPersistT :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> SqlBackend -> m a
runSqlPersistT (PersistT m) = runSqlConn m
{-# INLINE runSqlPersistT #-}

instance MonadTransControl (PersistT backend) where
  type StT (PersistT backend) a = StT (ReaderT SqlBackend) a
  liftWith = defaultLiftWith PersistT (\(PersistT m) -> m)
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT PersistT
  {-# INLINE restoreT #-}
instance MonadBaseControl b m => MonadBaseControl b (PersistT backend m) where
  type StM (PersistT backend m) a = ComposeSt (PersistT backend) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance MonadReader r m => MonadReader r (PersistT backend m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f (PersistT m) = PersistT $ mapReaderT (local f) m
  {-# INLINE local #-}
  reader = lift . reader
  {-# INLINE reader #-}

-- | This typeclass is a simple enumeration of the functions available from
-- "Database.Persist.Class", simply parameterized over the monad (rather than
-- requiring 'ReaderT' over 'MonadIO'). This makes it easier to use @persistent@
-- in an arbitrary monad transformer stack. Each method corresponds directly to
-- a function or method defined in "Database.Persist.Class" or
-- "Database.Persist.Sql". See the documentation for those modules for
-- information about what each method does.
--
-- This typeclass is primarily implemented by the 'PersistT' monad transformer,
-- which abstracts away the role of threading a database connection through your
-- program. If you are using a SQL database, it is recommended that you use the
-- 'SqlPersistT' alias and the corresponding 'runSqlPersistT' function in order
-- to avoid unnecessarily confusing type errors and to ensure all database
-- access is performed within a transaction.
--
-- This module provides 'MonadPersist' instances for various commonly-used monad
-- transformers, but it’s possible that you may want to use a monad transformer
-- from a different library that does not have an appropriate instance. In this
-- case, 'MonadPersist' provides default instances for all monad transformers
-- using the @DefaultSignatures@ GHC extension, which allows you to write empty
-- instance declarations to make any 'MonadTrans' instance an instance of
-- 'MonadPersist':
--
-- @
-- instance MonadPersist m => MonadPersist (T m)
-- @
class Monad m => MonadPersist backend m | m -> backend where
  -- PersistStore
  get :: (PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m (Maybe record)

  insert :: (PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m (Key record)
  insert_ :: (PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m ()
  insertMany :: (PersistStoreWrite backend, PersistRecordBackend record backend) => [record] -> m [Key record]
  insertMany_ :: (PersistStoreWrite backend, PersistRecordBackend record backend) => [record] -> m ()
  insertEntityMany :: (PersistStoreWrite backend, PersistRecordBackend record backend) => [Entity record] -> m ()
  insertKey :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  repsert :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  replace :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  delete :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> m ()
  update :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> [Update record] -> m ()
  updateGet :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> [Update record] -> m record

  getJust :: (PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m record
  belongsTo :: (PersistStoreRead backend, PersistRecordBackend record1 backend, PersistRecordBackend record2 backend) => (record1 -> Maybe (Key record2)) -> record1 -> m (Maybe record2)
  belongsToJust :: (PersistStoreRead backend, PersistRecordBackend record1 backend, PersistRecordBackend record2 backend) => (record1 -> Key record2) -> record1 -> m record2
  insertEntity :: (PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m (Entity record)

  -- PersistUnique
  getBy :: (PersistUniqueRead backend, PersistRecordBackend record backend) => Unique record -> m (Maybe (Entity record))

  deleteBy :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => Unique record -> m ()
  insertUnique :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Maybe (Key record))
  upsert :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> [Update record] -> m (Entity record)

  getByValue :: (PersistUniqueRead backend, PersistRecordBackend record backend) => record -> m (Maybe (Entity record))
  insertBy :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Either (Entity record) (Key record))
  replaceUnique :: (PersistUniqueWrite backend, PersistRecordBackend record backend, Eq record, Eq (Unique record)) => Key record -> record -> m (Maybe (Unique record))
  checkUnique :: (PersistUniqueRead backend, PersistRecordBackend record backend) => record -> m (Maybe (Unique record))
  onlyUnique :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Unique record)

  -- PersistQuery
  selectFirst :: (PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
  count :: (PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> m Int

  updateWhere :: (PersistQueryWrite backend, PersistRecordBackend record backend) => [Filter record] -> [Update record] -> m ()
  deleteWhere :: (PersistQueryWrite backend, PersistRecordBackend record backend) => [Filter record] -> m ()

  selectList :: (PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m [Entity record]
  selectKeysList :: (PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m [Key record]

  -- DeleteCascade
  deleteCascade :: DeleteCascade record backend => Key record -> m ()
  deleteCascadeWhere :: (DeleteCascade record backend, PersistQueryWrite backend) => [Filter record] -> m ()

  -- Database.Persist.Sql
  parseMigration :: (backend ~ SqlBackend) => Migration -> m (Either [Text] CautiousMigration)
  parseMigration' :: (backend ~ SqlBackend) => Migration -> m CautiousMigration
  printMigration :: (backend ~ SqlBackend) => Migration -> m ()
  showMigration :: (backend ~ SqlBackend) => Migration -> m [Text]
  getMigration :: (backend ~ SqlBackend) => Migration -> m [Sql]
  runMigration :: (backend ~ SqlBackend) => Migration -> m ()
  runMigrationSilent :: (backend ~ SqlBackend) => Migration -> m [Text]
  runMigrationUnsafe :: (backend ~ SqlBackend) => Migration -> m ()

  rawExecute :: (backend ~ SqlBackend) => Text -> [PersistValue] -> m ()
  rawExecuteCount :: (backend ~ SqlBackend) => Text -> [PersistValue] -> m Int64
  rawSql :: (backend ~ SqlBackend, RawSql a) => Text -> [PersistValue] -> m [a]
  transactionSave :: (backend ~ SqlBackend) => m ()
  transactionUndo :: (backend ~ SqlBackend) => m ()

  default get :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m (Maybe record)
  default insert :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m (Key record)
  default insert_ :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m ()
  default insertMany :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => [record] -> m [Key record]
  default insertMany_ :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => [record] -> m ()
  default insertEntityMany :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => [Entity record] -> m ()
  default insertKey :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  default repsert :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  default replace :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> record -> m ()
  default delete :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> m ()
  default update :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> [Update record] -> m ()
  default updateGet :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> [Update record] -> m record
  default getJust :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m record
  default belongsTo :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreRead backend, PersistRecordBackend record1 backend, PersistRecordBackend record2 backend) => (record1 -> Maybe (Key record2)) -> record1 -> m (Maybe record2)
  default belongsToJust :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreRead backend, PersistRecordBackend record1 backend, PersistRecordBackend record2 backend) => (record1 -> Key record2) -> record1 -> m record2
  default insertEntity :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m (Entity record)
  default getBy :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueRead backend, PersistRecordBackend record backend) => Unique record -> m (Maybe (Entity record))
  default deleteBy :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => Unique record -> m ()
  default insertUnique :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Maybe (Key record))
  default upsert :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> [Update record] -> m (Entity record)
  default getByValue :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueRead backend, PersistRecordBackend record backend) => record -> m (Maybe (Entity record))
  default insertBy :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Either (Entity record) (Key record))
  default replaceUnique :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend, Eq record, Eq (Unique record)) => Key record -> record -> m (Maybe (Unique record))
  default checkUnique :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueRead backend, PersistRecordBackend record backend) => record -> m (Maybe (Unique record))
  default onlyUnique :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => record -> m (Unique record)
  default selectFirst :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
  default count :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> m Int
  default updateWhere :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryWrite backend, PersistRecordBackend record backend) => [Filter record] -> [Update record] -> m ()
  default deleteWhere :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryWrite backend, PersistRecordBackend record backend) => [Filter record] -> m ()
  default selectList :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m [Entity record]
  default selectKeysList :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> m [Key record]
  default deleteCascade :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, DeleteCascade record backend) => Key record -> m ()
  default deleteCascadeWhere :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, DeleteCascade record backend, PersistQueryWrite backend) => [Filter record] -> m ()
  default parseMigration :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m (Either [Text] CautiousMigration)
  default parseMigration' :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m CautiousMigration
  default printMigration :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m ()
  default showMigration :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m [Text]
  default getMigration :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m [Sql]
  default runMigration :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m ()
  default runMigrationSilent :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m [Text]
  default runMigrationUnsafe :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Migration -> m ()
  default rawExecute :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Text -> [PersistValue] -> m ()
  default rawExecuteCount :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => Text -> [PersistValue] -> m Int64
  default rawSql :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend, RawSql a) => Text -> [PersistValue] -> m [a]
  default transactionSave :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => m ()
  default transactionUndo :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, backend ~ SqlBackend) => m ()

  get a = lift $ get a
  insert a = lift $ insert a
  insert_ a = lift $ insert_ a
  insertMany a = lift $ insertMany a
  insertMany_ a = lift $ insertMany_ a
  insertEntityMany a = lift $ insertEntityMany a
  insertKey a b = lift $ insertKey a b
  repsert a b = lift $ repsert a b
  replace a b = lift $ replace a b
  delete a = lift $ delete a
  update a b = lift $ update a b
  updateGet a b = lift $ updateGet a b
  getJust a = lift $ getJust a
  belongsTo a b = lift $ belongsTo a b
  belongsToJust a b = lift $ belongsToJust a b
  insertEntity a = lift $ insertEntity a
  getBy a = lift $ getBy a
  deleteBy a = lift $ deleteBy a
  insertUnique a = lift $ insertUnique a
  upsert a b = lift $ upsert a b
  getByValue a = lift $ getByValue a
  insertBy a = lift $ insertBy a
  replaceUnique a b = lift $ replaceUnique a b
  checkUnique a = lift $ checkUnique a
  onlyUnique a = lift $ onlyUnique a
  selectFirst a b = lift $ selectFirst a b
  count a = lift $ count a
  updateWhere a b = lift $ updateWhere a b
  deleteWhere a = lift $ deleteWhere a
  selectList a b = lift $ selectList a b
  selectKeysList a b = lift $ selectKeysList a b
  deleteCascade a = lift $ deleteCascade a
  deleteCascadeWhere a = lift $ deleteCascadeWhere a
  parseMigration a = lift $ parseMigration a
  parseMigration' a = lift $ parseMigration' a
  printMigration a = lift $ printMigration a
  showMigration a = lift $ showMigration a
  getMigration a = lift $ getMigration a
  runMigration a = lift $ runMigration a
  runMigrationSilent a = lift $ runMigrationSilent a
  runMigrationUnsafe a = lift $ runMigrationUnsafe a
  rawExecute a b = lift $ rawExecute a b
  rawExecuteCount a b = lift $ rawExecuteCount a b
  rawSql a b = lift $ rawSql a b
  transactionSave = lift transactionSave
  transactionUndo = lift transactionUndo

  {-# INLINE get #-}
  {-# INLINE insert #-}
  {-# INLINE insert_ #-}
  {-# INLINE insertMany #-}
  {-# INLINE insertMany_ #-}
  {-# INLINE insertEntityMany #-}
  {-# INLINE insertKey #-}
  {-# INLINE repsert #-}
  {-# INLINE replace #-}
  {-# INLINE delete #-}
  {-# INLINE update #-}
  {-# INLINE updateGet #-}
  {-# INLINE getJust #-}
  {-# INLINE belongsTo #-}
  {-# INLINE belongsToJust #-}
  {-# INLINE insertEntity #-}
  {-# INLINE getBy #-}
  {-# INLINE deleteBy #-}
  {-# INLINE insertUnique #-}
  {-# INLINE upsert #-}
  {-# INLINE getByValue #-}
  {-# INLINE insertBy #-}
  {-# INLINE replaceUnique #-}
  {-# INLINE checkUnique #-}
  {-# INLINE onlyUnique #-}
  {-# INLINE selectFirst #-}
  {-# INLINE count #-}
  {-# INLINE updateWhere #-}
  {-# INLINE deleteWhere #-}
  {-# INLINE selectList #-}
  {-# INLINE selectKeysList #-}
  {-# INLINE deleteCascade #-}
  {-# INLINE deleteCascadeWhere #-}
  {-# INLINE parseMigration #-}
  {-# INLINE parseMigration' #-}
  {-# INLINE printMigration #-}
  {-# INLINE showMigration #-}
  {-# INLINE getMigration #-}
  {-# INLINE runMigration #-}
  {-# INLINE runMigrationSilent #-}
  {-# INLINE runMigrationUnsafe #-}
  {-# INLINE rawExecute #-}
  {-# INLINE rawExecuteCount #-}
  {-# INLINE rawSql #-}
  {-# INLINE transactionSave #-}
  {-# INLINE transactionUndo #-}

#if MIN_VERSION_persistent(2,6,0)
  upsertBy :: (PersistUniqueWrite backend, PersistRecordBackend record backend) => Unique record -> record -> [Update record] -> m (Entity record)
  default upsertBy :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistUniqueWrite backend, PersistRecordBackend record backend) => Unique record -> record -> [Update record] -> m (Entity record)
  upsertBy a b c = lift $ upsertBy a b c
  {-# INLINE upsertBy #-}
#endif
#if MIN_VERSION_persistent(2,6,1)
  getJustEntity :: (PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m (Entity record)
  getEntity :: (PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> m (Maybe (Entity record))
  insertRecord :: (PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m record

  default getJustEntity :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreRead backend, PersistRecordBackend record backend) => Key record -> m (Entity record)
  default getEntity :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => Key record -> m (Maybe (Entity record))
  default insertRecord :: (Monad (t m'), MonadPersist backend m', MonadTrans t, t m' ~ m, PersistStoreWrite backend, PersistRecordBackend record backend) => record -> m record

  getJustEntity a = lift $ getJustEntity a
  getEntity a = lift $ getEntity a
  insertRecord a = lift $ insertRecord a

  {-# INLINE getJustEntity #-}
  {-# INLINE getEntity #-}
  {-# INLINE insertRecord #-}
#endif

-- | A simple alias that specializes 'MonadPersist' to 'SqlBackend'.
type MonadSqlPersist = MonadPersist SqlBackend

instance (MonadIO m, MonadBaseControl IO m) => MonadPersist backend (PersistT backend m) where
  get a = PersistT $ Sql.get a
  insert a = PersistT $ Sql.insert a
  insert_ a = PersistT $ Sql.insert_ a
  insertMany a = PersistT $ Sql.insertMany a
  insertMany_ a = PersistT $ Sql.insertMany_ a
  insertEntityMany a = PersistT $ Sql.insertEntityMany a
  insertKey a b = PersistT $ Sql.insertKey a b
  repsert a b = PersistT $ Sql.repsert a b
  replace a b = PersistT $ Sql.replace a b
  delete a = PersistT $ Sql.delete a
  update a b = PersistT $ Sql.update a b
  updateGet a b = PersistT $ Sql.updateGet a b
  getJust a = PersistT $ Sql.getJust a
  belongsTo a b = PersistT $ Sql.belongsTo a b
  belongsToJust a b = PersistT $ Sql.belongsToJust a b
  insertEntity a = PersistT $ Sql.insertEntity a
  getBy a = PersistT $ Sql.getBy a
  deleteBy a = PersistT $ Sql.deleteBy a
  insertUnique a = PersistT $ Sql.insertUnique a
  upsert a b = PersistT $ Sql.upsert a b
  getByValue a = PersistT $ Sql.getByValue a
  insertBy a = PersistT $ Sql.insertBy a
  replaceUnique a b = PersistT $ Sql.replaceUnique a b
  checkUnique a = PersistT $ Sql.checkUnique a
  onlyUnique a = PersistT $ Sql.onlyUnique a
  selectFirst a b = PersistT $ Sql.selectFirst a b
  count a = PersistT $ Sql.count a
  updateWhere a b = PersistT $ Sql.updateWhere a b
  deleteWhere a = PersistT $ Sql.deleteWhere a
  selectList a b = PersistT $ Sql.selectList a b
  selectKeysList a b = PersistT $ Sql.selectKeysList a b
  deleteCascade a = PersistT $ Sql.deleteCascade a
  deleteCascadeWhere a = PersistT $ Sql.deleteCascadeWhere a
  parseMigration a = PersistT $ Sql.parseMigration a
  parseMigration' a = PersistT $ Sql.parseMigration' a
  printMigration a = PersistT $ Sql.printMigration a
  showMigration a = PersistT $ Sql.showMigration a
  getMigration a = PersistT $ Sql.getMigration a
  runMigration a = PersistT $ Sql.runMigration a
  runMigrationSilent a = PersistT $ Sql.runMigrationSilent a
  runMigrationUnsafe a = PersistT $ Sql.runMigrationUnsafe a
  rawExecute a b = PersistT $ Sql.rawExecute a b
  rawExecuteCount a b = PersistT $ Sql.rawExecuteCount a b
  rawSql a b = PersistT $ Sql.rawSql a b
  transactionSave = PersistT Sql.transactionSave
  transactionUndo = PersistT Sql.transactionUndo

  {-# INLINE get #-}
  {-# INLINE insert #-}
  {-# INLINE insert_ #-}
  {-# INLINE insertMany #-}
  {-# INLINE insertMany_ #-}
  {-# INLINE insertEntityMany #-}
  {-# INLINE insertKey #-}
  {-# INLINE repsert #-}
  {-# INLINE replace #-}
  {-# INLINE delete #-}
  {-# INLINE update #-}
  {-# INLINE updateGet #-}
  {-# INLINE getJust #-}
  {-# INLINE belongsTo #-}
  {-# INLINE belongsToJust #-}
  {-# INLINE insertEntity #-}
  {-# INLINE getBy #-}
  {-# INLINE deleteBy #-}
  {-# INLINE insertUnique #-}
  {-# INLINE upsert #-}
  {-# INLINE getByValue #-}
  {-# INLINE insertBy #-}
  {-# INLINE replaceUnique #-}
  {-# INLINE checkUnique #-}
  {-# INLINE onlyUnique #-}
  {-# INLINE selectFirst #-}
  {-# INLINE count #-}
  {-# INLINE updateWhere #-}
  {-# INLINE deleteWhere #-}
  {-# INLINE selectList #-}
  {-# INLINE selectKeysList #-}
  {-# INLINE deleteCascade #-}
  {-# INLINE deleteCascadeWhere #-}
  {-# INLINE parseMigration #-}
  {-# INLINE parseMigration' #-}
  {-# INLINE printMigration #-}
  {-# INLINE showMigration #-}
  {-# INLINE getMigration #-}
  {-# INLINE runMigration #-}
  {-# INLINE runMigrationSilent #-}
  {-# INLINE runMigrationUnsafe #-}
  {-# INLINE rawExecute #-}
  {-# INLINE rawExecuteCount #-}
  {-# INLINE rawSql #-}
  {-# INLINE transactionSave #-}
  {-# INLINE transactionUndo #-}

#if MIN_VERSION_persistent(2,6,0)
  upsertBy a b c = PersistT $ Sql.upsertBy a b c
  {-# INLINE upsertBy #-}
#endif
#if MIN_VERSION_persistent(2,6,1)
  getJustEntity a = PersistT $ Sql.getJustEntity a
  getEntity a = PersistT $ Sql.getEntity a
  insertRecord a = PersistT $ Sql.insertRecord a

  {-# INLINE getJustEntity #-}
  {-# INLINE getEntity #-}
  {-# INLINE insertRecord #-}
#endif

instance MonadPersist backend m => MonadPersist backend (ExceptT e m)
instance MonadPersist backend m => MonadPersist backend (ReaderT r m)
instance (Monoid w, MonadPersist backend m) => MonadPersist backend (RWST r w s m)
instance MonadPersist backend m => MonadPersist backend (StateT s m)
instance (Monoid w, MonadPersist backend m) => MonadPersist backend (WriterT w m)

instance MonadPersist backend m => MonadPersist backend (LoggingT m)
instance MonadPersist backend m => MonadPersist backend (NoLoggingT m)
