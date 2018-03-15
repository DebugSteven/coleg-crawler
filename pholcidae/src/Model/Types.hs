module Model.Types where

import           ClassyPrelude
import           Database.Esqueleto.Internal.Sql
import           Database.Persist.Sql
import           Text.Blaze

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m, Functor m) => ReaderT SqlBackend m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val)
  , SqlSelect (SqlExpr (Entity val)) (Entity val)
  )

-- type SqlE e = SqlExpr (Entity e)

fetchThingByField
  :: (PersistField typ, DBVal val)
  => EntityField val typ -> typ -> DB (Maybe (Entity val))
fetchThingByField field u =
  selectFirst [field ==. u] []

newtype Address1 =
  Address1 { unAddress1 :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup Address1 where
  toMarkup = string . unpack . unAddress1

newtype Address2 =
  Address2 { unAddress2 :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup Address2 where
  toMarkup = string . unpack . unAddress2
