{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DataKinds #-} 

module Handler.Message where

import Import

import Yesod.Auth

import Data.Time (getCurrentTime)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (length, pack, unpack)
import Data.String (IsString)
import Text.Julius (rawJS, toJavascript)
import qualified Data.Aeson as J
import Data.HashMap.Strict ((!))
import Data.Aeson.Types (parse)


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC (pack, unpack, length)
import qualified Database.Esqueleto as E hiding (SqlBackend)


import qualified Data.ByteString.Base64 as B64 -- RFC 4648 compliant
import qualified Data.ByteString.Base16 as B16

import Crypto.Random
import Crypto.PBKDF.ByteString (sha256PBKDF2)

import Utils

-- Form widget to manipulate message creation from List Message User.
messageForm :: UserId -> Maybe UserId -> Html -> MForm Handler (FormResult (Message, Maybe ByteString), Widget)
messageForm owner maybeRecipient extra = do
  mu <- lift $ maybeAuth
  case mu of 
    Just (Entity _ userSession) -> do
      (recipientRes, recipientView) <- mreq textField (FieldSettings { 
                fsLabel = "Contacts", 
                fsTooltip = Nothing,
                fsId = Nothing,
                fsName = Nothing,
                fsAttrs = [("ng-model","to"), 
                           ("ng-change","compose.getUserInfo()"), 
                           ("ng-disabled","compose.userIsFound()"),
                           ("class", "form-control"),
                           ("placeholder", "Enter Recipient")]
               }) Nothing
      (bodyRes, bodyView) <- mreq textField (FieldSettings { 
                fsLabel = "Body", 
                fsTooltip = Nothing,
                fsId = Nothing,
                fsName = Nothing,
                fsAttrs = [("ng-model","body"), ("class", "form-control"), ("placeholder", "Type your message")]
               }) Nothing
      (passRes, passView) <- mopt passwordField "Secret Key" Nothing
      -- evaluates if there is a recipient value from argument (the message form is being generated from a created conversation
      recipient <- case maybeRecipient of 
      -- if there is a value in the 'recipient' input field, it returns the value,  wrapped with Maybe and FormResult Functors. 
        Just res -> return $ FormSuccess (Just res)
      -- If there is nothing in the maybeRecipient (the conversation is new), it creates a new conversation with the intended recipient
        Nothing -> do 
          case recipientRes of   
            FormSuccess rec -> do
              rec' <- lift $ runDB $ getBy (UniqueUsername rec)
              case rec' of 
                Just rec'' -> return $ FormSuccess (Just $ entityKey rec'')
                Nothing -> do
                  setMessage $ toHtml ("User not Found" :: Text)
                  redirect NewConversationR         
            FormFailure _ -> do
              setMessage $ toHtml ("User not Found" :: Text)
              redirect NewConversationR
            FormMissing -> return $ FormSuccess Nothing
      now  <- liftIO getCurrentTime
      salt <- liftIO $ generateSalt 32
      let secretKey  =  fmap (fmap ((\password -> (sha256PBKDF2 password salt 4 32))  . BC.pack . T.unpack)) passRes
      let body =  fmap (Just . BC.pack . T.unpack) bodyRes
      let length = fmap T.length bodyRes  --length Int 
      let messageRes = Message  <$>  body --body Text 
                                 <*> length --TODO: This must be a Maybe Value
                                 <*> pure owner -- owner UserId
                                 <*> recipient -- conversationWith UserId Maybe
                                 <*> pure owner -- sender UserId  
                                 <*> recipient  --recipient Maybe UserId 
                                 <*> pure now --createdAt UTCTime
                                 <*> pure True -- isNew
                                 <*> pure Nothing
                                 <*> pure Nothing
                                 <*> secretKey
                                 <*> pure Nothing
                                 <*> pure Nothing
          sessionUsername = userLogin userSession
          widget = $(widgetFile "message-form")
      return ((,) <$> messageRes <*> secretKey, widget)
    Nothing -> error "Cannot send message right now"



getConversationR :: UserId -> Handler Html 
getConversationR recipientId = do
  mu <- maybeAuthId 
  case mu of
    Just userId -> do
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
          (widget, enctype) <- generateFormPost $ messageForm userId (Just recipientId)
          defaultLayout $ do 
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
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
      (widget, enctype) <- generateFormPost $ messageForm  userId Nothing
      defaultLayout $(widgetFile "message")
      


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
          provideJson $ map (entityVal . fst) entityMessages

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
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
            addScript $ StaticR js_forge_forge_bundle_js
            addScript $ StaticR js_angular_js 
            $(widgetFile "outbox")
          provideJson $ map (entityVal . fst) entityMessages

-- Handles new chat request

getNewConversationR :: Handler Html
getNewConversationR = do 
  muser <- maybeAuthId
  case muser of 
    Just userId -> do  
      (widget, enctype) <- generateFormPost $ messageForm userId Nothing
      defaultLayout $ do 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
        addScript $ StaticR js_forge_forge_bundle_js
        addScript $ StaticR js_angular_js 
        --addScript $ StaticR js_controllers_app_js  
        $(widgetFile "compose") 
         
    Nothing  -> do
      setMessage $ toHtml ("Please, sign in to your account" :: Text)
      redirect HomeR      
      
-- Creates a new message and conversation (conversation has many messages)
postNewConversationR :: Handler Html
postNewConversationR = do 
  mu <- maybeAuthId 
  case mu of 
    Just user -> do 
      ((res, widget), enctype)  <- runFormPost $ messageForm user Nothing
      case res of 
        FormSuccess (messageRes, _) -> do
          let newChat = Chat {       chatOwner = user, 
                                      chatRecipient = fromJust $ messageRecipient messageRes,  
                                      chatKey = Nothing, -- Pass in ByteString
                                      chatCreatedAt = messageCreatedAt messageRes,
                                      chatIsNew = True
                                     }
          maybeChat <- runDB $ getBy $ UniqueChat user (chatRecipient newChat)
          (newChatId, newChatIdDup) <- case maybeChat of 
            Nothing  -> runDB $ do 
              newChatId <- insert newChat
              newChatIdDup <- insert $ newChat { 
                                            chatOwner = fromJust $ messageRecipient messageRes, 
                                            chatRecipient = user  
                                          }
              return (newChatId, newChatIdDup)
            Just chat -> do
              maybeDup <- runDB $ getBy $ UniqueChat (chatRecipient newChat) user
              case maybeDup of 
                Nothing ->  error "Unexpected Error while retrieving chat information"
                Just dup -> return (entityKey chat, entityKey dup)
          _ <- runDB $ insert $ messageRes {
                                            messageMedia = Nothing,
                                            messageFromChat = Just newChatId
                                            }
          _ <- runDB $ insert $ messageRes { 
                                                                                        
                                            messageOwner = fromJust $ messageRecipient messageRes,
                                            messageConversationWith = Just $ messageOwner messageRes,
                                            messageMedia = Nothing,
                                            messageFromChat = Just newChatIdDup
                                            }
          redirect (ConversationR $ fromJust $ messageRecipient messageRes)
        _              -> do
          setMessage $ toHtml ("There's been an error while sending your message. Please, try again" :: Text)
          redirect NewConversationR
                          
    Nothing -> do 
      setMessage $ toHtml ("Please, sign in to your account" :: Text)
      redirect HomeR     

      
postNewMessageR :: UserId -> Handler Html
postNewMessageR recipient = do 
  mu <- maybeAuthId 
  case mu of 
    Just user -> do 
      ((res, widget), enctype)  <- runFormPost $ messageForm user (Just recipient)
      case res of 
        FormSuccess (messageRes, passRes) -> do
          chatId <- runDB $ getBy $ UniqueChat user recipient
          chatIdDup <- runDB $ getBy $ UniqueChat recipient user
          _ <- runDB $ insert $ messageRes {
                                            messageMedia = Nothing,
                                            messageFromChat =fmap entityKey chatId 
                                            }
          _ <- runDB $ insert $ messageRes { 
                                            messageOwner = fromJust $ messageRecipient messageRes,
                                            messageConversationWith = Just $ messageOwner messageRes,
                                            messageMedia = Nothing,
                                            messageFromChat = fmap entityKey chatIdDup
                                            }
--          (widget, enctype) <- generateFormPost $ messageForm user
          redirect (ConversationR $ fromJust $ messageRecipient messageRes)
        _              -> do
          setMessage $ toHtml ("There's been an error while sending your message. Please, try again" :: Text)
          redirect NewConversationR
                          
    Nothing -> do 
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
    --TODO: Refactor it and add exhaustive cases for each json value
      let (J.Success createdAtTS) =  J.fromJSON (jsonRes  ! "createdAt")  :: J.Result Integer
      let (J.Success body) =  J.fromJSON (jsonRes  ! "body")  :: J.Result ByteString
      let (J.Success bodyCopy) =  J.fromJSON (jsonRes  ! "copy")  :: J.Result ByteString      
      let (J.Success to)  = J.fromJSON (jsonRes ! "to") :: J.Result Text
      let (J.Success from) =  J.fromJSON (jsonRes  ! "from")  :: J.Result Text
      let (J.Success authmessage) =  J.fromJSON (jsonRes  ! "authMessage")  :: J.Result Text
      maybeUserTo <- runDB $ getBy (UniqueUsername to)
      maybeUserFrom <- runDB $ getBy (UniqueUsername from)
    --TODO: Same as before, this must be refactored and cases must be taken in account      
      let (Entity keyTo valueTo) = fromJust maybeUserTo
      let (Entity keyFrom valueFrom) = fromJust maybeUserFrom
    --TODO: Conversation must be created if not exist. Otherwise, fetch it and add key value to 

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
                          messageAuthMessage = Nothing
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
                                        messageFromChat = Just recipientChat                                       
                                        }
      newCopyMess <- runDB $ insert duplicateMessage
      newMess <- runDB $ insert message
      
      -- Insert message duplicate for sender
      returnJson jsonRes
    J.Success _ -> error "unspecified datatype"
  

putMessageR :: MessageId -> Handler Html
putMessageR _ = error "Not implemented yet"

deleteMessageR :: MessageId -> Handler Html
deleteMessageR _ = error "Not implemented yet"

   
