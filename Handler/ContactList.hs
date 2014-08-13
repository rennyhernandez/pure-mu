module Handler.ContactList where

import Import
import Yesod.Auth
import Data.Time.Clock (getCurrentTime)
import Control.Monad 
import Data.Maybe (fromJust)
import qualified Database.Esqueleto as SQL
import qualified Prelude (head)
import Handler.User (recreatePublicKey)

--getContactsR :: Handler Html
--getContactsR = defaultLayout $(widgetFile "contacts")

getContactsR :: Handler Html
getContactsR  = do
  muser <- maybeAuth
  case muser of 
    Just (Entity userId user) -> do
      
      (list, contacts) <- runDB $ do
        contacts' <- SQL.select $
          SQL.from $ \(list, contactRef, user) -> do
          SQL.where_ $ (list SQL.^. ListOwner SQL.==. SQL.val userId SQL.&&.
                         list SQL.^. ListId SQL.==. contactRef SQL.^. ContactList SQL.&&.
                         contactRef SQL.^. ContactContact SQL.==. user SQL.^. UserId
                         )
          return user
        list' <- getBy $ UniqueList userId         
        return (list', contacts')                     
      defaultLayout $ do 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js" 
        $(widgetFile "contacts")
    Nothing -> do
     setMessage $ toHtml ("Please sign in to your account"  :: Text)
     redirect HomeR        

getContactR :: Text -> Handler Html
getContactR _ = error "getContact:GET"


putContactR :: Text -> Handler Html
putContactR _ = error "getContact:PUT"

deleteContactR :: Text -> Handler Html
deleteContactR login = do
  muser <- maybeAuth
  maybeContact <- runDB $ getBy $ UniqueUsername login
  case muser of 
    Just (Entity loggedId logged) -> do -- retrieves authenticated user
      case maybeContact of 
        Just (Entity contactId contact) -> do  
          maybeList <- runDB $ getBy $ UniqueList loggedId -- retrieves contact to be deleted from database
          case maybeList of 
            Just (Entity listId _) -> do
              runDB $ deleteBy $ UniqueContact listId contactId 
              maybeContactList <- runDB $ getBy $ UniqueList contactId -- retrieves contact list to delete its copy
              let (Entity contactList _) = fromJust maybeContactList
              runDB $ deleteBy $ UniqueContact contactList loggedId 
              setMessage $ toHtml ("Contact deleted successfully" :: Text)
              redirect ContactsR
            Nothing -> error "Could not retrieve list"
        Nothing -> error "Could not retrive contact"
    Nothing -> error "AuthError"


contactForm :: Form (Maybe Text, Maybe Text, Maybe Text)
contactForm = renderDivs $ (,,)
    <$> aopt textField "E-mail:" Nothing
    <*> aopt textField "Phone No." Nothing
    <*> aopt textField "Username" Nothing
-- 


getNewContactR ::  Handler Html
getNewContactR  =  do
    (formWidget, formEnctype) <- generateFormPost contactForm
    let userRes = Nothing :: Maybe (Entity User)
    defaultLayout $(widgetFile "new-contact")

postNewContactR :: Handler Html 
postNewContactR = do 
  ((result ,formWidget), formEnctype) <- runFormPost contactForm
  userRes <- do 
   case result of 
      FormSuccess (maybeEmail, maybePhone, maybeLogin) -> do
          case maybeEmail of
            Just email -> runDB $ getBy $ UniqueUser email 
            Nothing -> do
              case maybePhone of
                Just phone -> runDB $ getBy $ UniqueNumber phone
                Nothing -> do
                  case maybeLogin of
                    Just login -> runDB $ getBy $ UniqueUsername login
                    Nothing ->  return (Nothing :: Maybe (Entity User))
      _ -> return (Nothing :: Maybe (Entity User))
  case userRes of 
    Nothing -> do
      setMessage $ toHtml ("User not found. Please, try again" :: Text)
      redirect NewContactR
    _ -> do
      defaultLayout $(widgetFile "new-contact")

postAddContactR :: Text -> Handler Html
postAddContactR login = do
  muser <- maybeAuth
  case muser of
    Just (Entity userId user) -> do 
      maybeAddedUser <- runDB $ getBy $ UniqueUsername login
      case maybeAddedUser of 
        Just (Entity addedUserId addedUser) -> do
          maybeList <- runDB $ getBy $ UniqueList userId
          addedUserList <- runDB $ getBy $ UniqueList addedUserId
          case maybeList of
            Just (Entity listId list) -> do
              now <- liftIO getCurrentTime
              _ <- runDB $ insert $ Contact listId addedUserId now
              _ <- runDB $ insert $ Contact (entityKey (fromJust addedUserList)) userId now
              setMessage $ toHtml  ("Contact Added Successfully" :: Text)
              redirect ContactsR
            Nothing -> do
              setMessage $ toHtml ("Could not add user at this time. Please, try again later" :: Text)
              redirect ContactsR
        Nothing -> do
          setMessage $ toHtml ("Could not add user at this time. Please, try again later" :: Text)
          redirect ContactsR
    Nothing -> do
      setMessage $ toHtml ("Could not add user at this moment. Please, try again later." :: Text)
      redirect ContactsR


--getContactListR :: UserId -> (List,[Contact])
getFullContactListR :: UserId -> Handler Value
getFullContactListR userId = do
   cl <- runDB $ SQL.select $
     SQL.from $ \(list, contact, user, keyring)  -> do
     SQL.where_ ( list SQL.^. ListOwner SQL.==. SQL.val userId SQL.&&.
                   contact SQL.^. ContactList SQL.==. list SQL.^. ListId SQL.&&.
                   user SQL.^. UserId SQL.==. contact SQL.^. ContactContact SQL.&&.
                   user SQL.^. UserId SQL.==. keyring SQL.^. KeyringOwner
                   )
     return (list, user, keyring SQL.^. KeyringPublicKey)
   let list = (entityVal . (\(x,_,_) -> x) . Prelude.head) cl -- gets the first element of contact list which has contact list information
   let contacts = map  (\(_,y,z) ->  (entityVal y, recreatePublicKey $ SQL.unValue z)) cl      
   returnJson $ (list, contacts)
   


