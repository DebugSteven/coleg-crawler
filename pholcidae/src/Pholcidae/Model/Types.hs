module Pholcidae.Model.Types where

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

newtype CodeNumber =
  CodeNumber { unCodeNumber :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup CodeNumber where
  toMarkup = string . unpack . unCodeNumber

newtype CodeTitle =
  CodeTitle { unCodeTitle :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup CodeTitle where
  toMarkup = string . unpack . unCodeTitle

newtype Committee = 
  Committee { unCommittee :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup Committee where
  toMarkup = string . unpack . unCommittee

newtype Description =
  Description { unDescription :: Text }
  deriving (Eq, Show, PersistField, PersistFieldSql)

instance ToMarkup Description where
  toMarkup = string . unpack . unDescription
