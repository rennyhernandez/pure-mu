{-# LANGUAGE OverloadedStrings #-} 

module Handler.Message where

import Import
import Data.Time (getCurrentTime)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (length)
import Yesod.Auth
import Data.String (IsString)
import qualified Database.Esqueleto as E hiding (SqlBackend)



  

 
-- Form widget to manipulate message creation from List Message User.
messageForm :: UserId -> Maybe UserId -> Html -> MForm Handler (FormResult (Message, Maybe Text), Widget)
messageForm owner maybeRecipient extra = do
  contacts' <- lift $ runDB $ E.select $
          E.from $ \(list,contact,user) -> do
          E.where_ $ (list E.^. ListOwner E.==. E.val owner E.&&.
                     contact E.^. ContactList E.==. list E.^. ListId E.&&.
                     user E.^. UserId E.==. contact E.^. ContactContact)
          return (user E.^. UserLogin)                             
  let contacts = map (\login -> (E.unValue login, E.unValue login)) contacts'
  (recipientRes, recipientView) <- mreq (selectFieldList contacts) "Contacts" Nothing
--  (recipientRes, recipientView) <- mreq textField "To:" Nothing
  (bodyRes, bodyView) <- mreq textField "Body" Nothing
  (passRes, passView) <- mopt passwordField "Secret Key" Nothing

  recipient <- case maybeRecipient of 
    Just res -> return $ FormSuccess (Just res)
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
  let messageRes = Message  <$> fmap Just bodyRes --body Text 
                             <*> fmap T.length bodyRes  --length Int
                             <*> pure owner -- owner UserId
                             <*> recipient -- conversationWith UserId Maybe
                             <*> pure owner -- sender UserId  
                             <*> recipient  --recipient Maybe UserId 
                             <*> pure now --createdAt UTCTime
                             <*> pure True -- isNew
                             <*> pure Nothing
                             <*> pure Nothing
      widget = $(widgetFile "message-form")
  return ((,) <$> messageRes <*> passRes, widget)    



getConversationR :: UserId -> Handler Html 
getConversationR recipientId = do
  mu <- maybeAuthId 
  case mu of
    Just userId -> do
      maybeRecipient <- runDB $ get recipientId 
      case maybeRecipient of 
        Just _ -> do
          conversation <- runDB $ E.select $ 
                          E.from $ \(message, sender) -> do 
                          E.where_ (message E.^. MessageOwner E.==. E.val userId E.&&. 
                                     message E.^. MessageConversationWith E.==.  E.just (E.val recipientId) E.&&. 
                                     message E.^. MessageSender E.==. sender E.^. UserId)
                          E.orderBy [E.asc (message E.^. MessageCreatedAt)]
                          return (message, sender)               
    {-      if (null conversation)
            then do
              
              redirect MessagesR        
            else do-}
          (widget, enctype) <- generateFormPost $ messageForm userId (Just recipientId)
          defaultLayout $(widgetFile "conversation")        
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
      


getMessagesR :: Handler Html
getMessagesR = do
  mid <- maybeAuthId 
  case mid of 
    Nothing -> redirect HomeR
    Just userId -> do
--      entityMessages <- runDB $ selectList [MessageOwner ==. userId]   [Asc MessageCreatedAt]  
      entityMessages <- runDB $ E.select $ 
                      E.from $ \(message, sender) -> do 
                      E.where_ (message E.^. MessageOwner E.==. E.val userId E.&&.                                
                                 message E.^. MessageRecipient E.==. E.just (message E.^.  MessageOwner) E.&&. 
                                 message E.^. MessageSender E.!=. E.val userId    E.&&.     
                                 message E.^. MessageSender E.==. sender E.^. UserId
                                 )
                      E.orderBy [E.asc (message E.^. MessageCreatedAt)]
                      return (message, sender)               
      defaultLayout $ do 
        $(widgetFile "inbox")

-- Handles new chat request

getNewConversationR :: Handler Html
getNewConversationR = do 
  muser <- maybeAuthId
  case muser of 
    Just userId -> do  
      (widget, enctype) <- generateFormPost $ messageForm userId Nothing
      defaultLayout $(widgetFile "compose") 
    Nothing  -> do
      setMessage $ toHtml ("Please, sign in to your account" :: Text)
      redirect HomeR      

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
          liftIO $ print $ messageRes
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





    

putMessageR :: MessageId -> Handler Html
putMessageR _ = error "Not implemented yet"

deleteMessageR :: MessageId -> Handler Html
deleteMessageR _ = error "Not implemented yet"

   
