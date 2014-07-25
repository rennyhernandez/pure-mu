{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)
import Yesod.Auth.HashDB (HashDBUser(..))


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance HashDBUser User where
  userPasswordHash = Just . userPassword
  setPasswordHash p u = u { userPassword = p }


