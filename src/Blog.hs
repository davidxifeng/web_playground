{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Blog
    (myblog)
    where

import           Control.Exception           (bracket)
import           Control.Monad               (msum, mzero)
import           Control.Monad.Trans         (MonadIO, liftIO)
import qualified Data.Text                   as T
import           Text.Blaze.Html5            (Html (), a, p, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5.Attributes as A

import           Happstack.Server            (BodyPolicy,
                                              Browsing (EnableBrowsing),
                                              Conf (port), CookieLife (Session),
                                              Method (GET, POST), Response,
                                              ServerPart, addCookie, decodeBody,
                                              defaultBodyPolicy, dir, dirs,
                                              lookText, method, mkCookie,
                                              nullConf, ok, path,
                                              readCookieValue, serveDirectory,
                                              simpleHTTP, toResponse)
import qualified Text.XmlHtml                as X

-- 加入初步的acid-state支持
import           Control.Applicative         ((<$>))
import           Control.Monad.Reader        (ask)
import           Control.Monad.State         (get, put)
import           Data.Acid                   (AcidState, Query, Update,
                                              makeAcidic)
                                                          --, query
import           Data.Acid.Advanced          (query', update')
import           Data.Acid.Local             (createCheckpointAndClose,
                                              openLocalState,
                                              openLocalStateFrom)
import           Data.Data                   (Data, Typeable)
import           Data.IxSet                  (Indexable (..), IxSet, Proxy (..),
                                              getOne, ixFun, ixSet, (@=))
import qualified Data.IxSet                  as IxSet
import           Data.SafeCopy               (SafeCopy, base, deriveSafeCopy)
import           Data.Time                   (UTCTime (..), getCurrentTime)

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

data Post = Post
    { title     :: T.Text
    , author    :: T.Text
    , post      :: T.Text
    --, time      :: UTCTime
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Post)

setPost :: T.Text -> Update Post Post
setPost t = do
    l@Post{..} <- get
    let ns = "Hello Acid"
    put $ l { title = ns, author = "david", post = t}
    return l

getPost :: Query Post T.Text
getPost = post <$> ask

$(makeAcidic ''Post ['setPost, 'getPost])


myblog :: ServerPart Response
myblog = do
    msum [viewForm, processForm]
    where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           acid <- liftIO $ openLocalStateFrom "blogState" (Post "a" "b" "c")
           p <- query' acid GetPost
           liftIO $ createCheckpointAndClose acid
           ok $ template "form" $
               H.form ! A.action "/blog" ! A.enctype "multipart/form-data" !  A.method "POST" $ do
                        H.label ! A.for "msg" $ "say you"
                        H.input ! A.type_ "text" ! A.id "msg" ! A.name "msg"
                        H.input ! A.type_ "submit" ! A.value "say it"
    processForm :: ServerPart Response
    processForm =
        do method POST
           decodeBody myPolicy
           acid <- liftIO $ openLocalStateFrom "blogState" (Post "a" "b" "c")
           _ <- update' acid (SetPost "Hello Jack")
           msg <- lookText "test"
           liftIO $ createCheckpointAndClose acid
           ok $ template "blog demo" $ do
                H.p "generate a new record:"
                H.p (toHtml msg)

template :: T.Text -> Html -> Response
template title bd = toResponse $
        H.html $ do
          H.head $ do
            H.title (toHtml title)
          H.body $ do
                bd
                p $ a ! href "/" $ "back to home"
                