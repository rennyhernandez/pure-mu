{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DataKinds #-} 

module Handler.Message where

import Import

import Yesod.Auth

import Data.Time (getCurrentTime)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (length, pack, unpack, append)
import Data.String (IsString)
import Text.Julius (rawJS, toJavascript)
import qualified Data.Aeson as J
import Data.HashMap.Strict ((!), fromList)
import Data.Aeson.Types (parse)


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC (pack, unpack, length)
import qualified Database.Esqueleto as E hiding (SqlBackend)


import qualified Data.ByteString.Base64 as B64 -- RFC 4648 compliant
import qualified Data.ByteString.Base16 as B16

import Crypto.Random
import Crypto.PBKDF.ByteString (sha256PBKDF2)

import Utils



getConversationR :: UserId -> Handler Html 
getConversationR recipientId = do
  mu <- maybeAuth
  case mu of
    Just (Entity userId _) -> do
      maybeRecipient <- runDB $ get recipientId 
      case maybeRecipient of     
        Just recipient -> do
          conversation <- runDB $ E.select $ 
                          E.from $ \(message, sender) -> do 
                          E.where_ (message E.^. MessageOwner E.==. E.val userId E.&&. 
                                     message E.^. MessageConversationWith E.==.  E.just (E.val recipientId) E.&&. 
                                     message E.^. MessageSender E.==. sender E.^. UserId)
                          E.orderBy [E.asc (message E.^. MessageCreatedAt)]
                          return (message, sender)               
          defaultLayout $ do
            setTitle $ toHtml $ "Chat with "  `T.append` (userLogin recipient)
            addScript $ StaticR js_forge_forge_bundle_js 
            addScript $ StaticR js_angular_js                     
            $(widgetFile "conversation")             
        Nothing -> redirectError "You cannot contact this user right now" MessagesR
    Nothing -> redirect MessagesR


  
getMessageR :: MessageId -> Handler Html
getMessageR messageId = do 
  mu <- maybeAuthId
  case mu of
    Nothing -> error "Error while opening logged user"
    Just userId -> do 
      maybeMessage <- runDB $ get messageId
      case maybeMessage of 
        Just message -> do
          let maybeRecipient = messageRecipient message        
              widget = $(widgetFile "message-form")
          defaultLayout $(widgetFile "message")
        Nothing -> redirectError  "Can't find message" MessagesR
      


getMessagesR :: Handler TypedContent
getMessagesR = do 
    mid <- maybeAuthId 
    case mid of 
      Nothing -> redirect HomeR
      Just userId -> do
        entityMessages <- runDB $ E.select $ 
                        E.from $ \(message, sender) -> do 
                        E.where_ (message E.^. MessageOwner E.==. E.val userId E.&&.                                
                                   message E.^. MessageRecipient E.==. E.just (message E.^.  MessageOwner) E.&&. 
                                   message E.^. MessageSender E.!=. E.val userId    E.&&.     
                                   message E.^. MessageSender E.==. sender E.^. UserId
                                   )
                        E.orderBy [E.asc (message E.^. MessageCreatedAt)]
                        return (message, sender)  
        selectRep $ do 
          provideRep $ defaultLayout $ do
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
            addScript $ StaticR js_forge_forge_bundle_js
            addScript $ StaticR js_angular_js 
            $(widgetFile "inbox")
          provideJson $ map fst entityMessages

getSentMessagesR :: Handler TypedContent
getSentMessagesR = do 
    mid <- maybeAuthId 
    case mid of 
      Nothing -> redirect HomeR
      Just userId -> do
        entityMessages <- runDB $ E.select $ 
                        E.from $ \(message, destination) -> do 
                        E.where_ (message  E.^. MessageOwner E.==. E.val userId E.&&.                                
                                   message E.^. MessageSender E.==. message E.^.MessageOwner E.&&.
                                   message E.^. MessageRecipient E.==. E.just (destination E.^. UserId)
                                   )
                        E.orderBy [E.asc (message E.^. MessageCreatedAt)]
                        return (message, destination)  
        selectRep $ do 
          provideRep $ defaultLayout $ do
            addScript $ StaticR js_forge_forge_bundle_js
            addScript $ StaticR js_angular_js 
            $(widgetFile "outbox")
          provideJson $ map fst entityMessages

-- Handles new chat request

getNewConversationR :: Handler Html
getNewConversationR = do 
  muser <- maybeAuthId
  case muser of 
    Just userId -> do  
      let maybeRecipient = Nothing
      defaultLayout $ do 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
        addScript $ StaticR js_forge_forge_bundle_js
        addScript $ StaticR js_angular_js 
        $(widgetFile "compose") 
    Nothing  -> do
      setMessage $ toHtml ("Please, sign in to your account" :: Text)
      redirect HomeR      
                                 
generateSalt :: Int -> IO ByteString
generateSalt byteSize = do 
  g <- newGenIO :: IO SystemRandom
  let eitherRand = genBytes byteSize g
  (salt, gen) <- case eitherRand of 
    Left _ ->  error "Could not generate random bytes"
    Right t -> return t
  return salt



{-
postMessageSend receives a POST request. The data received with the request has the form: 
   { to : String, 
     from: String, 
     body: String, 
     createdAt: Number
   }


-}
postMessageSendR :: Handler Value
postMessageSendR = do 
  maybeValue <- parseJsonBody :: Handler (J.Result Value)
  case maybeValue of
    J.Error s -> error "Could not parse json body" --TODO: set errors when defined 
    J.Success (J.Object jsonRes) -> do 
      liftIO $ print jsonRes
    --TODO: Refactor it and add exhaustive cases for each json value
      let (J.Success createdAtTS) =  J.fromJSON (jsonRes  ! "createdAt")  :: J.Result Integer
      let (J.Success body) =  J.fromJSON (jsonRes  ! "body")  :: J.Result ByteString
      let (J.Success bodyCopy) =  J.fromJSON (jsonRes  ! "copy")  :: J.Result ByteString      
      let (J.Success to)  = J.fromJSON (jsonRes ! "to") :: J.Result Text
      let (J.Success from) =  J.fromJSON (jsonRes  ! "from")  :: J.Result Text
      let (J.Success authmessage) =  J.fromJSON (jsonRes  ! "authMessage")  :: J.Result Text
{-      let (J.Success fileType) =  J.fromJSON (jsonRes  ! "fileType")  :: J.Result Text
      let (J.Success fileSize) =  J.fromJSON (jsonRes  ! "fileSize")  :: J.Result Text
      maybeFile = J.fromJSON (jsonRes ! "fileContent") :: J.Result Text
      file <- do 
        case maybeFile of 
          J.Success file <- Just file
          J.Error _ <- Nothing
-}      
      maybeUserTo <- runDB $ getBy (UniqueUsername to)
      maybeUserFrom <- runDB $ getBy (UniqueUsername from)
    --TODO: Same as before, this must be refactored and cases must be taken in account      
      let (Entity keyTo valueTo) = fromJust maybeUserTo
      let (Entity keyFrom valueFrom) = fromJust maybeUserFrom
    --TODO: Conversation must be created if not exist. Otherwise, fetch it and add key value to 

    -- Message Attachment must be stored twice (original and duplicate for the sender)
    
    -- Attachment is created and inserted if filecontent request parameter is not null



    -- Chat information for the recipient
      maybeChat <- runDB $ getBy (UniqueChat keyTo keyFrom)
      chat <- case maybeChat of 
        Just (Entity chatId _) -> return chatId
        Nothing -> do 
          chatId <- runDB $ insert $ Chat {  chatOwner = keyTo,
                                             chatRecipient = keyFrom,
                                             chatIsNew = True,
                                             chatKey = Nothing,
                                             chatCreatedAt = parseJSTimestamp createdAtTS
                                             }
          return chatId      
     -- Message for the recipient  
      let message = Message {
                          messageBody = Just body,
                          messageLength = BC.length body,
                          messageCreatedAt = parseJSTimestamp createdAtTS,
                          messageMedia = Nothing,
                          messageConversationWith = Just keyFrom,
                          messageIsNew = True,
                          messageRecipient = Just keyTo, 
                          messageSender = keyFrom,
                          messageOwner = keyTo,
                          messageFromChat = Just chat,
                          messageSecretKey = Nothing,
                          messageSalt = Nothing,
                          messageAuthMessage = Nothing,
                          messageStatus = "Validated" -- TODO: Change when dealing with message lifecycle
                          }
      -- Duplicate information for recipient
      maybeRecipientChat <- runDB $ getBy (UniqueChat keyFrom keyTo)                          
      recipientChat <- case maybeRecipientChat of 
        Just (Entity chatId _) -> return chatId
        Nothing -> do 
          chatId <- runDB $ insert $ Chat {  chatOwner = keyFrom,
                                             chatRecipient = keyTo,
                                             chatIsNew = True,
                                             chatKey = Nothing,
                                             chatCreatedAt = parseJSTimestamp createdAtTS
                                             }
          return chatId                                
      let duplicateMessage = message {  
                                        messageBody = Just bodyCopy,
                                        messageLength = BC.length bodyCopy, 
                                        messageConversationWith = Just keyTo,
                                        messageIsNew = True,
                                        messageRecipient = Just keyTo,
                                        messageSender = keyFrom,
                                        messageOwner = keyFrom,
                                        messageFromChat = Just
                                        recipientChat                                       
                                        }
                                        
                                        
      newCopyMess <- runDB $ insert duplicateMessage -- message must be duplicated using sender's private key 
      newMess <- runDB $ insert message
      
      returnJson jsonRes
    J.Success _ -> error "unspecified datatype"

-- creates a new nonce and seqid for the message to be created
getMessageRequestR :: Handler Value
getMessageRequestR = do
  mid <- maybeAuthId
  case mid of
    Just _ -> do 
      nonce <- liftIO $ generateSalt 32
      returnJson $ fromList [("nonce" :: Text, B64.encode nonce)]
    Nothing -> do
      returnJson ("Unathorized" :: Text)
    
putMessageR :: MessageId -> Handler Html
putMessageR _ = error "Not implemented yet"

deleteMessageR :: MessageId -> Handler Html
deleteMessageR _ = error "Not implemented yet"

   
