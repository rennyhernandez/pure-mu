{-# LANGUAGE OverloadedStrings #-}

module Handler.SignUp where

import Import

import Crypto.Hash.SHA256 (hash)
import Crypto.Random  
import Codec.Crypto.RSA
import Crypto.PBKDF.ByteString (sha256PBKDF2)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.Text as T
import Crypto.Types.PubKey.RSA 
import qualified Data.Binary as BIN (encode, decode, Binary)
import Text.Printf
        
getSignUpR :: Handler Html
getSignUpR = do 
  mu <- maybeAuthId 
  case mu of 
    Nothing -> do
      (widget, enctype) <- generateFormPost userForm 
      defaultLayout
        [whamlet|
          <form method=post action=@{SignUpR} enctype=#{enctype}>
            ^{widget}               
            <button>Submit
      |]
    Just _ -> do
      redirect HomeR
      
      
getFormResultValue :: FormResult Text -> String
getFormResultValue fr = case fr of
  FormSuccess a -> T.unpack a
  _ -> error "Not a valid password"
  
createKeyring :: IO (PublicKey, PrivateKey)
createKeyring = do
    g <- liftIO $ newGenIO :: IO SystemRandom
    let (pubk, pvtk, _) = generateKeyPair g 2048  
    return (pubk, pvtk)
-- TODO: Serialize PublicKey and PrivateKey Types
-- TODO: Find a solution to export private and public keys values to another environment using JSON

generateSalt :: IO B.ByteString       
generateSalt = do 
  g <- newGenIO :: IO SystemRandom
  let res = genBytes 32 g -- 32 * 8 bits 
  case res of
    Right (x, _) -> return x
    Left  _      -> error "Couldn't generate salt"
    
hashPasswordBS :: B.ByteString -> String -> B.ByteString
hashPasswordBS saltBS pass =  hash $ BC.pack saltedPass
  where saltedPass = pass ++ (BC.unpack saltBS)
--  where saltedPass = pass `T.append` ((T.pack . BC.unpack) saltBS)

--hashPassword :: Text -> Text -> Text
--hashPassword salt pass = T.pack $ BC.unpack $ B64.encode (hash saltedPass)
--hashPassword salt pass = T.pack $ BC.unpack $ hash saltedPass  
--  where saltedPass = BC.pack $ T.unpack (pass `T.append` salt)

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
  
-- Utils for handling bytestring, text and binary values

packKeys :: BIN.Binary a => a -> Text
packKeys bytes = T.pack $ BCL.unpack $ BIN.encode bytes

unpackKeys :: BIN.Binary a => Text -> a
unpackKeys text = BIN.decode $ BCL.pack $ T.unpack text



userForm :: Html ->  MForm Handler (FormResult User, Widget)
userForm  extra  = do 
  (usernameRes, usernameView) <- mreq textField FieldSettings
    { fsLabel = "Login"
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = []
    } Nothing
  (emailRes, emailView) <- mreq emailField "Email" Nothing
  (passwordRes, passwordView) <- mreq passwordField "Password" Nothing
  (nameRes, nameView) <- mreq textField "Name" Nothing
  (countryRes, countryView) <- mreq textField "Country" Nothing  
  (phoneNoRes, phoneNoView) <- mreq textField "Phone No." Nothing  
  salt <- liftIO $ (T.pack . BC.unpack) <$> generateSalt
  createdAt <- liftIO getCurrentTime
  aesKey <- liftIO generateAESKey
  let userRes = User <$> usernameRes
                      <*> emailRes
                      <*> (hashPassword salt <$> passwordRes)  
--                    <*> pure generateSalt
                      <*> pure salt
                      <*> pure "verkey"
                      <*> pure False
                      <*> nameRes
                      <*> countryRes
                      <*> phoneNoRes
                      <*> pure createdAt
                      <*> pure aesKey
      widget = $(widgetFile "user-form")
  return (userRes, widget)

hashPassword :: Text -> Text -> Text 
-- hashPassword salt password = T.pack $ hashedSaltedPassword >>= printf "%02x"
hashPassword salt password = T.pack hashedSaltedPassword
  where
    saltedPassword = T.unpack $ salt `T.append` password
    hashedSaltedPassword = BC.unpack $ hash (BC.pack saltedPassword)
    
toHex :: Text -> Text
toHex text = T.pack $ printf "%02x" unpackedString
  where unpackedString = T.unpack text
    
postSignUpR :: Handler Html
postSignUpR = do 
((res, userWidget), enctype) <- runFormPost userForm
case res of 
   FormSuccess userRes -> do      
     userId <- runDB $ insert userRes
     now <- liftIO $ getCurrentTime
     contactList <- runDB $ insert $ List { listOwner = userId, 
                                             listCreatedAt = now,
                                             listLastUpdated = Nothing
                                           }
     (pubk, pvtk, _) <- liftIO $ genUserKeyring
     let encodedPublicKey = T.pack $ BCL.unpack $ BIN.encode pubk
     let encodedPrivateKey = T.pack $ BCL.unpack $ BIN.encode pvtk
     keyringId <- runDB $ insert $ Keyring encodedPublicKey encodedPrivateKey userId
     redirect $ UserR userId       
   FormFailure errors -> do
        defaultLayout $ do 
           $(widgetFile "home")
   _ -> do 
           defaultLayout $ do 
            [whamlet|<p> Otherwise |]

