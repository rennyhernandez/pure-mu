module Handler.User where

import Import
import Prelude (head)
import Yesod.Auth
import Yesod.Auth.HashDB hiding (User, UserId, UniqueUser, userSalt, userPassword)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Codec.Binary.Base64.String as B64
import Crypto.Types.PubKey.RSA
import Codec.Crypto.RSA
import Crypto.PasswordStore
import Crypto.Random  
import qualified Data.Binary as BIN (encode, decode, Binary)
import Data.Time (UTCTime, getCurrentTime)
import qualified Crypto.Hash.SHA256 as H
import qualified Database.Esqueleto as SQL
import Data.Typeable (typeOf)
import Utils

getUserR :: UserId -> Handler Html
getUserR userId = do
  (user, keyring) <- runDB $ do
    user <- get404 userId
    keyring <- selectList [KeyringOwner ==. userId] [LimitTo 1] -- TODO: Refactor 
    return (user, entityVal $ head keyring)
  defaultLayout $ do 
    $(widgetFile "user")

getUsersR :: Handler Html
getUsersR = do 
  users <- runDB $ selectList [] [Asc UserCreatedAt]
  defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
    $(widgetFile "users")
    
getDeleteUserR :: UserId -> Handler Html
getDeleteUserR userId = error $  "Sorry, "  ++ (show userId) ++ "cannot be deleted, yet!"
  
deleteUserR :: UserId -> Handler Html
deleteUserR userId = do
  runDB $ deleteWhere [KeyringOwner ==. userId]
  runDB $ delete userId
  redirect UsersR
  
getSignUpR :: Handler Html
getSignUpR = do
  mu <- maybeAuthId
  case mu of 
    Nothing -> do
      defaultLayout $ do 
        setTitle "Pure :: Create Account"
        $(widgetFile "signup")
    Just _ -> redirect HomeR

postSignUpR :: Handler Html
postSignUpR = do
  now <- liftIO $ getCurrentTime
  aesKey <- liftIO $ generateAESKey
  let password = ireq passwordField "password" 
  user <- runInputPost $ User <$> ireq textField "username"
                            <*> ireq textField "email"
                            <*> password
                            <*> pure ""
                            <*> pure "verkey"
                            <*> pure False
                            <*> ireq textField "name"
                            <*> ireq textField "country"
                            <*> ireq textField "phone"
                            <*> pure now
                            <*> pure aesKey
  --defaultLayout [whamlet| <p> #{show user} |]     
  userId <- runDB $ insert user -- Insert User
  now <- liftIO $ getCurrentTime 
  contactList <- runDB $ insert $ List { listOwner = userId, -- Create Contact List
                                         listCreatedAt = now,
                                         listLastUpdated = now
                                       }
  (pubk, pvtk, _) <- liftIO $ genUserKeyring --Generate User Keyring for encryption
  let pubkey = T.pack $ BCL.unpack $ BIN.encode pubk
  let pvtkey = T.pack $ BCL.unpack $ BIN.encode pvtk
  _ <- runDB $ insert $ Keyring pubkey pvtkey "" userId
  redirect $ UserR userId       
    
{-
userForm :: Html ->  MForm Handler (FormResult User, Widget)
userForm  extra  = do 
 
  passwd <- do
   case passwordRes of  -- FormResult
    FormSuccess pass -> do 
      pass' <- liftIO $ makePassword  ((BC.pack . T.unpack) pass) 14 --PBKDF2 
      return $ (T.pack . BC.unpack) pass'     
    _ -> return "" 
  createdAt <- liftIO  getCurrentTime
  aesKey <- liftIO generateAESKey
--  let textSalt =  (T.pack . BC.unpack)  $ exportSalt salt -- ^ salt
    userRes <- runInputGet $ User <$> usernameRes
                      <*> emailRes
                      <*> pure passwd 
--                    <*> pure generateSalt
                      <*> pure ""
                      <*> pure "verkey"
                      <*> pure False
                      <*> nameRes
                      <*> countryRes
                      <*> phoneNoRes
                      <*> pure createdAt
                      <*> pure aesKey
      widget = $(widgetFile "user-form")
  return (userRes, widget)


postSignUpR :: Handler Html
postSignUpR = do 
  muser <- maybeAuth
  
  userRes <- runInputGet $ User <$> usernameRes
                      <*> emailRes
                      <*> pure passwd 
--                    <*> pure generateSalt
                      <*> pure ""
                      <*> pure "verkey"
                      <*> pure False
                      <*> nameRes
                      <*> countryRes
                      <*> phoneNoRes
                      <*> pure createdAt
                      <*> pure aesKey
  case res of 
     FormSuccess userRes -> do      
       userId <- runDB $ insert userRes -- Insert User
       now <- liftIO $ getCurrentTime 
       contactList <- runDB $ insert $ List { listOwner = userId, -- Create Contact List
                                               listCreatedAt = now,
                                               listLastUpdated = now
                                             }
       (pubk, pvtk, _) <- liftIO $ genUserKeyring --Generate User Keyring for encryption
       let pubkey = T.pack $ BCL.unpack $ BIN.encode pubk
       let pvtkey = T.pack $ BCL.unpack $ BIN.encode pvtk
       _ <- runDB $ insert $ Keyring pubkey pvtkey "" userId
       redirect $ UserR userId       
     FormFailure errors -> do
          defaultLayout $ do 
             $(widgetFile "home")
     _ -> do 
             defaultLayout $ do 
              [whamlet|<p> Otherwise |]
-}
getPublicKeyR :: Text -> Handler Value
getPublicKeyR username = do 
  pubkeys <- runDB $ SQL.select $ 
    SQL.from $ \(user,keyring) -> do  
    SQL.where_ ( user SQL.^. UserLogin SQL.==. SQL.val username SQL.&&.
                  user SQL.^. UserId SQL.==. keyring SQL.^. KeyringOwner)
    return(user, keyring SQL.^. KeyringPublicKey)
  returnJson $ (recreatePublicKey . SQL.unValue . snd . head) pubkeys


getPrivateKeyR :: Text -> Handler Value
getPrivateKeyR username = do 
  privateKeys <- runDB $ SQL.select $ 
    SQL.from $ \(user,keyring) -> do  
    SQL.where_ ( user SQL.^. UserLogin SQL.==. SQL.val username SQL.&&.
                  user SQL.^. UserId SQL.==. keyring SQL.^. KeyringOwner)
    return(keyring SQL.^. KeyringPrivateKey)
--debugging:  liftIO $ print $ (recreatePrivateKey . SQL.unValue . head) privateKeys 
  returnJson $ (recreatePrivateKey . SQL.unValue . head) privateKeys    


getFullUserInfoR :: Text -> Handler Value
getFullUserInfoR username = do
  fullUserInfos <- runDB $ SQL.select $
    SQL.from $ \(user, keyring) -> do
    SQL.where_ (user SQL.^. UserLogin SQL.==. SQL.val username SQL.&&.
                user SQL.^. UserId SQL.==. keyring SQL.^. KeyringOwner)
    return(user, keyring SQL.^. KeyringPublicKey)
  let fullUserInfo = head fullUserInfos -- SQL.select returns an array of results, taking first element
  now <- liftIO getTimestamp 
  liftIO $ print now
  liftIO $ print fullUserInfo
  returnJson $ (entityVal . fst)  fullUserInfo

-- Public and PrivateKeys are encoded and stored in binary format. These functions 
-- decode them from binary and construct their appropiate type

recreatePublicKey :: Text -> PublicKey
recreatePublicKey pubkey = BIN.decode $ BCL.pack $ T.unpack pubkey

recreatePrivateKey :: Text -> PrivateKey
recreatePrivateKey pvtkey = BIN.decode $ BCL.pack $ T.unpack pvtkey

--
-- 


createKeyring :: IO (PublicKey, PrivateKey)
createKeyring = do
    g <- liftIO $ newGenIO :: IO SystemRandom
    let (pubk, pvtk, _) = generateKeyPair g 2048  
    return (pubk, pvtk)

genUserKeyring :: IO (PublicKey, PrivateKey, SystemRandom)
genUserKeyring = do 
  g <- newGenIO :: IO SystemRandom
  let (pubk, pvtk, gen) = generateKeyPair g 2048 -- for RSA 2048 use 
  return $ (pubk,pvtk,gen)

generateAESKey :: IO B.ByteString
generateAESKey = do 
  g <- newGenIO :: IO SystemRandom
  let res = genBytes 32 g -- 32 * 8  = 256 bits
  case res of
    Right (x, _) -> return x
    Left  _      -> error "Couldn't generate AES Key"

    
    

    


  
