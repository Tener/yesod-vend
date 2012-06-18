{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, ExistentialQuantification, DefaultSignatures, QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Yesod.VEND
-- Copyright   :  2012 Krzysztof Skrzętnicki <gtener@gmail.com>
-- License     :  BSD-style (see the LICENSE file in the distribution)
--  
-- Maintainer  :  Krzysztof Skrzętnicki <gtener@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--  
-- This module provides simple typeclass 'CRUD' which implements CRUD (or VEND: View Edit New Delete) functionality for Yesod.
-- 
-- There are default implementations that are unfortunately filtered out by Haddock. The implementation uses DefaultSignatures extension to not force a specific implementation on a library user.
--  
-- An example is given below.
-- 
-- Suppose we have a User entity defined as config/models:
-- 
-- > User
-- >     ident Text
-- >     name Text Maybe
-- >     address Text Maybe
-- >     telephone Text Maybe
-- 
-- Our module would then start with: 
-- 
-- > {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- > module Handler.User where
-- >  
-- > import Import
-- > import Yesod.VEND
-- >  
-- > import Data.Maybe
-- 
-- Define helper datatype.
-- 
-- > data UserP = UserP
-- 
-- Provide routing. This assumes that config/routes reads:
-- 
-- > /user/new                    UserNewR    
-- > /user/edit/#UserId           UserEditR   
-- > /user/delete/#UserId         UserDeleteR 
-- > /user/view/all               UserViewAllR   
-- > /user/view/single/#UserId    UserViewR   
-- 
-- Methods:
-- 
-- > handleUserNewR = newR UserP
-- > handleUserDeleteR = deleteR UserP
-- > handleUserEditR = editR UserP
-- > handleUserViewR = viewR UserP
-- > handleUserViewAllR = viewAllR UserP
-- 
-- Define 'EntityDeep' instance for UserId. We use default implementations.
-- 
-- > instance EntityDeep UserId where
-- >     type EntT UserId = User
-- >     type FullEntT UserId = User
-- 
-- 
-- Define 'CRUD' instance for our helper type UserP. Define 'ValT' and 'KeyT' types.
-- 
-- > instance CRUD UserP where
-- >     type ValT UserP = User
-- >     type KeyT UserP = UserId
-- >  
-- 
-- Wire routing information in:
-- 
-- >     newRt _ = UserNewR
-- >     editRt _ = UserEditR
-- >     deleteRt _ = UserDeleteR
-- >     viewRt _ = UserViewR
-- >     viewAllRt _ = UserViewAllR
-- >  
-- 
-- Define which 'parameters' will be displayed for entity and how:
-- 
-- >     params _ = [(EntityParam "Ident" userIdent id markupToWidget)
-- >                ,(EntityParam "Name" userName mns mnsw)
-- >                ,(EntityParam "Address" userAddress mns mnsw)
-- >                ,(EntityParam "Telephone" userTelephone mns mnsw)
-- >                ] where mns = fromMaybe "not set"
-- >                        mnsw = maybe [whamlet|<i>not set</i>|] markupToWidget
-- >  
-- 
-- Specify sorting in 'view all' view:
-- 
-- >     viewAllOptions _ = [Asc UserId]
-- >  
-- 
-- Define entity name:
-- 
-- >     entName _ = "User"
-- 
-- Form for creating/editing entity:
-- 
-- >     form _ proto = return $ renderDivs $  
-- >                     User
-- >                       <$> areq textField "Identifier" (fmap userIdent proto)
-- >                       <*> aopt textField "Name" (fmap userName proto)
-- >                       <*> aopt textField "Address" (fmap userAddress proto)
-- >                       <*> aopt textField "Telephone" (fmap userTelephone proto)

module Yesod.VEND where

import Yesod

import Text.Hamlet (shamlet)
import Data.Text(Text)
import Text.Blaze.Html

import qualified Data.Text
import Database.Persist.GenericSql.Raw

-- | Datatype for providing different views on specific datatype. Transforms the parameter into intermediate datatype b from which one can use 'epToText' to get 'Text' or 'epToWidget' to get a 'GWidget'.
data EntityParam master sub a = forall b . EntityParam { epName :: Text
                                                       , epGet :: (a -> b)
                                                       , epToText :: (b -> Text)
                                                       , epToWidget :: (b -> GWidget master sub ()) }

-- * Helpers to work around problems with existential types.
--   see: http://stackoverflow.com/questions/10192663/why-cant-existential-types-use-record-syntax
-- 

-- | We cannot use record syntax to access fields of existential types. Instead we have:
-- 
-- >  epGetText (EntityParam _ pGet pToText _) = pToText . pGet
epGetText :: EntityParam t t1 t2 -> t2 -> Text
epGetText (EntityParam _ pGet pToText _) = pToText . pGet
-- | We cannot use record syntax to access fields of existential types. Instead we have:
-- 
-- > epGetWidget (EntityParam _ pGet _ pToWidget) = pToWidget . pGet
epGetWidget :: EntityParam t t1 t2 -> t2 -> GWidget t t1 ()
epGetWidget (EntityParam _ pGet _ pToWidget) = pToWidget . pGet

-- | Class for accessing entities referenced by 'a' entity type. For example for entities Foo, Bar:
-- 
-- > Foo
-- >     name Text
-- > Bar
-- >     size Int
-- >     foo  FooId
-- 
-- We would have this for Bar:
-- 
-- > instance EntityDeep BarId where
-- >     type EntT = Bar
-- >     type FullEntT = (Bar,Foo)
-- >  
-- >     get404Full key = runDB $ do
-- >                    v1 <- get404 key
-- >                    v2 <- get404 (barFoo v1)
-- >                    return (v1,v2)
-- >  
-- >     entityCore _ = fst
-- >     paramsFull _ = [(EntityParam "Size" (barSize . fst) id markupToWidget)
-- >                    ,(EntityParam "Foo's name" (fooName . snd) id markupToWidget)]
-- >       

class EntityDeep a where
    -- | 'base' entity type. Not critically needed but useful.
    type EntT a :: *
    type EntT a = a

    -- | 'full' entity type.
    type FullEntT a :: *
    type FullEntT a = a


    -- | get 'full' entity from base. default implementation works akin to 'get404'.
    get404Full :: a -> GHandler master sub (FullEntT a)
    -- | return 'base' type from 'full' type
    entityCore :: a -> (FullEntT a) -> (EntT a)
    -- | get a list of parameters describing the 'full' type
    paramsFull :: a -> [EntityParam master sub (FullEntT a)]

    default entityCore :: (EntT a ~ FullEntT a) => a -> (FullEntT a) -> (EntT a)
    entityCore _ = id
    default paramsFull :: ((CRUD a), (ValT a ~ FullEntT a)) => a -> [EntityParam master sub (FullEntT a)]
    paramsFull = params
    default get404Full :: ((YesodPersistBackend sub ~ SqlPersist),
                           (PersistEntity val0),
                           (YesodPersist sub),
                           (a ~ Key SqlPersist val0),
                           (val0 ~ FullEntT (Key SqlPersist val0))) => 
                           a -> GHandler master sub (FullEntT a)
    get404Full key = runDB (get404 key)

-- | Given description of entity parameters ('EntityParam' list) and terse/not terse option return a widget displaying the entity.
displayEntityWidget :: a -> [EntityParam master sub a] -> Bool -> GWidget master sub ()
displayEntityWidget a pars terse | terse = [whamlet| 
 $forall ep <- pars
   <td> ^{epGetWidget ep a} |]
                                 | otherwise = [whamlet|
$forall ep <- pars
 <p> #{epName ep}: 
       <span .param> ^{epGetWidget ep a}
 |]

-- | Core typeclass of this package. Default implementations of handlers use other methods to provide sensible default views. They can be all overriden if needed.
class (EntityDeep (KeyT a)) => CRUD a where
    -- * types 
    -- | entity value type
    type ValT a
    -- | entity key type
    type KeyT a

    -- * stupid methods, we cant just use (undefined :: KeyT a) because of how type families work.
    -- | provide a value of type 'KeyT a'. Default implementation is 'undefined'.
    getSomeKey :: a -> KeyT a
    getSomeKey _ = undefined
    -- | provide a value of type 'ValT a'. Default implementation is 'undefined'.
    getSomeVal :: a -> ValT a 
    getSomeVal _ = undefined

    -- fixme: better way to handle this?
    -- | used for sorting entities in 'view all'
    viewAllOptions :: a -> [SelectOpt (ValT a)]
    viewAllOptions _ = []


    -- * routes
    -- | route to 'new element'
    newRt     :: a -> Route site
    -- | route to 'view all elements'
    viewAllRt :: a -> Route site
    -- | route to 'view element'
    viewRt    :: a -> (KeyT a) -> Route site
    -- | route to 'delete element'
    deleteRt  :: a -> (KeyT a) -> Route site
    -- | route to 'edit element'
    editRt    :: a -> (KeyT a) -> Route site

    -- * displaying
    -- | provide widget for displaying an element. Bool argument specifies if this is for \"terse\" view or not.
    displayWidget       :: a -> (ValT a) -> Bool -> GWidget master sub ()
    -- | provide widget for displaying element header. Used in 'view all'.
    displayHeaderWidget :: a -> Bool -> GWidget master sub ()
    -- | simple version of 'paramsFull' only for 'ValT' type.
    params              :: a -> [EntityParam master sub (ValT a)]

    -- * names
    -- | entity name. this will be changed in future versions to support proper internationalization.
    entName :: a -> Text

    -- * forms
    -- | form for creating new entity/editing old one.
    form  :: a -> (Maybe (ValT a)) -> GHandler master sub (Html -> MForm master sub (FormResult (ValT a), (GWidget master sub ())) )
    -- | deletion form.
    dForm :: a -> GHandler master sub (Html -> MForm master sub (FormResult Bool, (GWidget master sub ())))

    -- * handlers
    -- | handler for 'viewRt'
    viewR    :: a -> (KeyT a) -> GHandler master sub RepHtml
    -- | handler for 'editRt'
    editR    :: a -> (KeyT a) -> GHandler master sub RepHtml
    -- | handler for 'newRt'
    newR     :: a -> GHandler master sub RepHtml
    -- | handler for 'deleteRt'
    deleteR  :: a -> (KeyT a) -> GHandler master sub RepHtml
    -- | handler for 'viewAllRt'
    viewAllR :: a -> GHandler master sub RepHtml

    -- default implementations

    default params :: (Show (ValT a)) => a -> [EntityParam master sub (ValT a)]
    params _ = [EntityParam "shown" show Data.Text.pack markupToWidget]

    default displayHeaderWidget :: a -> Bool -> GWidget master sub ()
    displayHeaderWidget this terse | terse = let pars = paramsFull (getSomeKey this) in [whamlet| 
<tr>
 <th colspan="20"> #{entName this}
<tr>
  <th>
      Actions
 $forall ep <- pars
  <th> #{epName ep}
 |]
                                  | otherwise = [whamlet|
<p> #{entName this} |]

    default displayWidget :: a -> (ValT a) -> Bool -> GWidget master sub ()
    displayWidget this a terse | terse = [whamlet| 
 $forall ep <- params this
   <td> #{epGetText ep a} |]
                               | otherwise = [whamlet|
<p> #{entName this}
$forall ep <- params this
 <p> #{epName ep}: 
       <span .param> #{epGetText ep a}
 |]

    default dForm :: (RenderMessage sub FormMessage) =>
                     a -> GHandler master sub (Html -> MForm master sub (FormResult Bool, (GWidget master sub ())))
    dForm _this = return $ renderDivs (areq areYouSureField "Are you sure?" (Just False))
        where areYouSureField = check isSure boolField
              isSure False = Left ("You must be sure." :: Text)
              isSure True = Right True


    default newR :: ((Yesod sub),
                     (YesodPersistBackend sub ~ SqlPersist),
                     (RenderMessage sub FormMessage),
                     (YesodPersist sub),
                     (KeyT a ~ Key SqlPersist (ValT a)),
                     (PersistEntity (ValT a))) => 
                     a -> GHandler master sub RepHtml
    newR this = do
      ((result, wg),et) <- runFormPost =<< (form this Nothing)
      let newForm = (wg,et)

      case result of
         FormSuccess val -> do
            key <- runDB $ insert val
            setMessage "Entity created."
            defaultLayout $ do
                 setTitle $ toHtml $ "Created: " ++ show (entName this) ++ show key -- FIXME -- better title
                 [whamlet| Creation completed. ID=#{show key}. <a href=@{viewRt this key}>View here</a>. <br>^{displayWidget this val False} |] -- FIXME - show key or not?
         _ ->  defaultLayout $ do
                 setTitle $ toHtml $ "New: " ++ show (entName this) -- FIXME -- better title
                 [whamlet| <p .message> New: #{entName this}
<form method=post action="" enctype=#{snd newForm}>
    ^{fst newForm}
    <input type="submit"> |]

    default viewAllR :: ((YesodPersistBackend sub ~ SqlPersist),
                         (YesodPersist sub),
                         (Yesod sub),
                         (EntityDeep (Key SqlPersist (ValT a))), 
                         (PersistEntityBackend (ValT a) ~ SqlPersist), 
                         (KeyT a ~ Key SqlPersist (ValT a)), 
                         (PersistEntity (ValT a))) => a -> GHandler master sub RepHtml
    viewAllR this = do
      values <- runDB $ selectList [] (viewAllOptions this)
      values'full <- mapM (\ k -> fmap (\ v -> (k,v)) (get404Full k)) (map entityKey values)
      terse <- getTerse
      defaultLayout $ do
             setTitle $ [shamlet| All items |] -- FIXME: better title
             [whamlet| 
Choose view: <a href="?terse=1">Terse</a> <a href="?terse=0">Wide</a>
<br>
<strong>
    <a href=@{newRt this}> Create new
<br>
<br>

$if terse
 <table .terse .table-striped .table .table-condensed>
  <thead>
   ^{displayHeaderWidget this terse}
  <tbody>
   $forall (key, val'full) <- values'full
    <tr>
     <td>
       <a href=@{viewRt this key}> View
       <a href=@{editRt this key}> Edit
       <a href=@{deleteRt this key}> <strong> Delete </strong>

     ^{displayEntityWidget val'full (paramsFull key) terse}
$else
  ^{displayHeaderWidget this terse}
  $forall (key, val'full) <- values'full
   <hr .listsep> 
   ^{displayEntityWidget val'full (paramsFull key) terse}
   <p .actions> Actions:
       <a href=@{viewRt this key}> View
       <a href=@{editRt this key}> Edit
       <a href=@{deleteRt this key}> <strong> Delete </strong>
       |]

    default viewR :: ((Yesod sub), (KeyT a ~ Key SqlPersist (ValT a)), (PersistEntity (ValT a))) => a -> (KeyT a) -> GHandler master sub RepHtml
    viewR this key = do
      val'full <- get404Full key
      defaultLayout $ do
             setTitle $ toHtml $ "View: " ++ show (entName this) ++ show key -- FIXME: better title
             [whamlet| 

^{displayEntityWidget val'full (paramsFull key) False}
<p .actions> Actions:
    <a href=@{newRt this}> New
    <a href=@{editRt this key}> Edit
    <a href=@{deleteRt this key}> <strong> Delete </strong>
    |]

    
    default editR :: ((YesodPersistBackend sub ~ SqlPersist),
                      (YesodPersist sub),
                      (Yesod sub),
                      (RenderMessage sub FormMessage),
                      (KeyT a ~ Key SqlPersist (ValT a)), (PersistEntity (ValT a))) => a -> (KeyT a) -> GHandler master sub RepHtml
    editR this key = do
      val <- runDB $ get404 key
      ((result,fwidget), enctype) <- runFormPost =<< (form this (Just val))
      case result of
         FormSuccess new'val -> do
            runDB $ replace key new'val
            return ()
         _ -> return ()

      defaultLayout $ do
         setTitle $ toHtml $ "Edit: " ++ show (entName this) ++ show key -- FIXME -- better title
         [whamlet| 
<p .actions> Actions:
    <a href=@{newRt this}> New
    <a href=@{viewRt this key}> View
    <a href=@{deleteRt this key}> <strong> Delete </strong>

<p .message> Edit: #{entName this}
<form method=post action="" enctype=#{enctype}>
    ^{fwidget}
    <input type="submit"> |]
 
    default deleteR :: ((RenderMessage sub FormMessage), (YesodPersist sub), (YesodPersistBackend sub ~ SqlPersist), (Yesod sub),
                        (KeyT a ~ Key SqlPersist (ValT a)), (PersistEntity (ValT a))) => a -> (KeyT a) -> GHandler master sub RepHtml
    deleteR this key = do
       val'full <- get404Full key
       ((result,fwidget), enctype) <- runFormPost =<< (dForm this)
       case result of
         FormSuccess _ -> do
            runDB $ delete key
            setMessage $ [shamlet|Deleted: #{entName this} #{show key} |] -- FIXME -- more data
            defaultLayout $ do
                 setTitle $ [shamlet|Deleted: #{entName this} #{show key} |] -- FIXME -- better title
                 [whamlet| Delete completed. |]
         _ ->  defaultLayout $ do
                 setTitle $ toHtml $ "Delete: " ++ show (entName this) ++ show key -- FIXME -- better title
                 [whamlet| <p .message> Delete: #{entName this}
^{displayEntityWidget val'full (paramsFull key) False}
<form method=post action="" enctype=#{enctype}>
    ^{fwidget}
    <input type="submit"> |]

-- * Utility functions

-- | Check if the view should be \"terse\" or not. Checks for \"terse\" in GET parameters. Default is True. 1 means True, 0 means False.
getTerse :: GHandler master sub Bool
getTerse = do
 p'terse <- lookupGetParam "terse"
 return $ case p'terse of
   Just "1" -> True
   Just "0" -> False
   _ -> True -- this makes terse default

-- | make 'GWidget' from any type that implements 'ToMarkup'
markupToWidget :: ToMarkup a => a -> GWidget sub master ()
markupToWidget t = [whamlet|#{t}|]

-- fst3 (v,_,_) = v
-- snd3 (_,v,_) = v
-- thr3 (_,_,v) = v
