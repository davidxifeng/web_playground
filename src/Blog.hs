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
import qualified Text.XmlHtml                as X

-- 加入初步的acid-state支持
import           Control.Applicative         ((<$>))
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

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

newtype MessageId = MessageId { unMessageId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

data Message = Message
    { messageId     :: MessageId
    , author    :: Text
    , message      :: Text
    , timestamp      :: UTCTime
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Message)

newtype Author      = Author Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Message where
    empty = ixSet [ ixFun $ \l -> [messageId l]
                  , ixFun $ \l -> [Author $ author l]
                  ]

data MessageDB = MessageDB
    { nextMessageId :: MessageId
    , messages      :: IxSet Message
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''MessageDB)

-- | 插入一条新的消息到数据库中
newMessage :: Message -> Update MessageDB ()
newMessage msg = do
    md@MessageDB{..} <- get
    put $ md { nextMessageId = MessageId 1
             , messages      = IxSet.insert msg messages
             }

getMessage :: Query MessageDB [Message]
getMessage = do
    MessageDB{..} <- ask
    return $ IxSet.toList messages

$(makeAcidic ''MessageDB ['newMessage, 'getMessage])

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

myblog :: ServerPart Response
myblog = do
    msum [viewForm, processForm]
    where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           acid <- liftIO $ openLocalState (MessageDB (MessageId 1) empty)
           ml <- query' acid (GetMessage)
           r <- ok $ template "form" $ do
               H.p ! A.class_ "text-info" $ "get blog"
               simpleForm
               mapM_ messageHtml ml
               H.p ! A.class_ "text-info" $ "end of messages"
           liftIO $ createCheckpointAndClose acid
           return r
    processForm :: ServerPart Response
    processForm =
        do method POST
           decodeBody myPolicy
           acid <- liftIO $ openLocalState (MessageDB (MessageId 1) empty)
           author' <- lookText' "author"
           msg' <- lookText' "message"
           time' <- liftIO $ getCurrentTime
           let msg = Message (MessageId 1) author' msg' time'
           _ <- update' acid (NewMessage msg)
           ml <- query' acid (GetMessage)
           r <- ok $ template "blog demo" $ do
                H.p ! A.class_ "text-error" $ "insert a new record to acid state"
                simpleForm
                mapM_ messageHtml ml
           liftIO $ createCheckpointAndClose acid
           return r

lookText' :: String -> ServerPart Text
lookText' = fmap toStrict . lookText

template :: Text -> Html -> Response
template title bd = toResponse $
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

