{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Control.Applicative (pure)
import Prelude
import Yesod
import Data.Text (Text, pack, unpack)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)
import Yesod.Auth.HashDB (HashDBUser(..))
import Data.Aeson (withText, Value(..), ToJSON(..), FromJSON(..))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BC (pack, unpack)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON ByteString where
  toJSON = String . pack . BC.unpack 
  
instance FromJSON ByteString where
 parseJSON = withText "ByteString" $  pure . BC.pack . unpack
 

instance HashDBUser User where
  userPasswordHash = Just . userPassword
  setPasswordHash p u = u { userPassword = p }


