{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Blog where
--import Control.Monad.Trans.Class (lift)
import           Control.Monad               (msum)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (toStrict)
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

-- 加入初步的acid-state支持
import           Control.Monad.Reader        (ask)
import           Control.Monad.State         (get, put)
import           Data.Acid                   (AcidState, Query, Update,
                                              makeAcidic)
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

import           BlogTypes
import           Types

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


messageHtml :: Message -> Html
messageHtml (Message{..}) =
    H.div ! A.class_ "message" $ do
        H.p ! A.class_ "text-info" $ do H.toHtml author >> "说:"
        H.p ! A.class_ "text-info" $ H.toHtml message
        H.p ! A.class_ "text-warning" $ do "at: " >> H.toHtml (show timestamp)
        H.br

simpleForm :: Html
simpleForm = do
    H.form ! A.action "/blog" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
        H.input ! A.type_ "text" ! A.id "author" ! A.name "author"
        H.input ! A.type_ "text" ! A.id "message" ! A.name "message"
        H.input ! A.type_ "submit" ! A.value "发表"

myblog :: App Response
myblog = do
    msum [viewForm, processForm]
    where
    viewForm :: App Response
    viewForm =
        do method GET
           acid <- liftIO $ openLocalState initMessageDB
           ml <- query' acid (GetAllMessages)
           r <- ok $ blogTemplate "form" $ do
               H.p ! A.class_ "text-info" $ "get blog"
               simpleForm
               mapM_ messageHtml ml
               H.p ! A.class_ "text-info" $ "end of messages"
           liftIO $ createCheckpointAndClose acid
           return r
    processForm :: App Response
    processForm =
        do method POST
           decodeBody myPolicy
           acid <- liftIO $ openLocalState initMessageDB
           author' <- lookText' "author"
           msg' <- lookText' "message"
           time' <- liftIO $ getCurrentTime
           let msg = Message (MessageId 1) author' msg' False time'
           _ <- update' acid (NewMessage msg)
           ml <- query' acid (GetAllMessages)
           r <- ok $ blogTemplate "blog demo" $ do
                H.p ! A.class_ "text-error" $ "insert a new record to acid state"
                simpleForm
                mapM_ messageHtml ml
           liftIO $ createCheckpointAndClose acid
           return r

lookText' :: String -> App Text
lookText' = fmap toStrict . lookText

blogTemplate :: Text -> Html -> Response
blogTemplate title bd = toResponse $
        H.html $ do
          H.head $ do
            H.link ! A.rel "stylesheet" ! A.href "css/eggplant/jquery-ui-1.9.1.custom.min.css"
            H.link ! A.rel "stylesheet" ! A.href "b/css/bootstrap.min.css"
            H.link ! A.rel "stylesheet" ! A.href "b/css/bootstrap-responsive.min.css"
            H.title (toHtml title)
          H.body $ do
                H.h1 "我的心情动态web系统"
                p "一组动态div"
                bd
                p $ a ! href "/" $ "back to home"

