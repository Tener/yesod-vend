{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, EmptyDataDecls #-}
import Yesod
import Database.Persist.Sqlite
import Yesod.VEND 
import Data.Maybe
import Data.Text(Text)
import Control.Applicative

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
User
    ident Text
    name Text Maybe
    address Text Maybe
    telephone Text Maybe
    deriving Show
|]

data VendUserTest = VendUserTest ConnectionPool

mkYesod "VendUserTest" [parseRoutes|
/user/new                    UserNewR    
/user/edit/#UserId           UserEditR   
/user/delete/#UserId         UserDeleteR 
/user/view/all               UserViewAllR   
/user/view/single/#UserId    UserViewR   
/                            HomeR
|]

instance Yesod VendUserTest

instance YesodPersist VendUserTest where
    type YesodPersistBackend VendUserTest = SqlPersist

    runDB action = do
        VendUserTest pool <- getYesod
        runSqlPool action pool
---
data UserP = UserP
instance EntityDeep UserId where
    type EntT UserId = User     
    type FullEntT UserId = User 
    type SiteEntT UserId = VendUserTest

    paramsFull _ = params UserP

instance CRUD UserP where    
    type ValT UserP = User   
    type KeyT UserP = UserId 
    type SiteT UserP = VendUserTest

    newRt _ = UserNewR        
    editRt _ = UserEditR      
    deleteRt _ = UserDeleteR  
    viewRt _ = UserViewR      
    viewAllRt _ = UserViewAllR

    params _ = [(EntityParam "Ident" userIdent id markupToWidget)           
               ,(EntityParam "Name" userName mns mnsw)                      
               ,(EntityParam "Address" userAddress mns mnsw)                
               ,(EntityParam "Telephone" userTelephone mns mnsw)            
               ] where mns = fromMaybe "not set"                            
                       mnsw = maybe [whamlet|<i>not set</i>|] markupToWidget

    viewAllOptions _ = [Asc UserId]

    entName _ = "User"

    form _ proto = return $ renderDivs $                                       
                    User                                                       
                      <$> areq textField "Identifier" (fmap userIdent proto)   
                      <*> aopt textField "Name" (fmap userName proto)          
                      <*> aopt textField "Address" (fmap userAddress proto)    
                      <*> aopt textField "Telephone" (fmap userTelephone proto)


handleUserNewR :: GHandler master VendUserTest RepHtml
handleUserDeleteR :: Key SqlPersist (UserGeneric SqlPersist) -> GHandler master VendUserTest RepHtml
handleUserEditR :: Key SqlPersist (UserGeneric SqlPersist) -> GHandler master VendUserTest RepHtml
handleUserViewR :: Key SqlPersist (UserGeneric SqlPersist) -> GHandler master VendUserTest RepHtml
handleUserViewAllR :: GHandler master VendUserTest RepHtml

handleUserNewR = newR UserP
handleUserDeleteR = deleteR UserP
handleUserEditR = editR UserP
handleUserViewR = viewR UserP
handleUserViewAllR = viewAllR UserP

handleHomeR :: Handler RepHtml
handleHomeR = do
    defaultLayout [whamlet|Hello World Dweller! Take a look at this:
<ol>
    <li> 
        <a href=@{newRt UserP}> New #{entName UserP}
    <li> 
        <a href=@{viewAllRt UserP}> View all #{entName UserP}s
|]

    
openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "test-usersite.db" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3030 $ VendUserTest pool

instance RenderMessage VendUserTest FormMessage where
    renderMessage _ _ = defaultFormMessage
