{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where
import           Control.Exception                       (bracket)
import qualified Control.Exception.Lifted                as Lifted

import           Control.Monad                           (MonadPlus, mplus,
                                                          msum, mzero)
import           Control.Monad.Trans                     (MonadIO, liftIO)
import qualified Data.Text                               as T
import           Happstack.Server.Heist                  (templateServe)
import           Text.Blaze.Html5                        (Html (), a, p, toHtml,
                                                          (!))
import qualified Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             (href)
import qualified Text.Blaze.Html5.Attributes             as A
import           Text.Templating.Heist                   (HeistT, Template,
                                                          bindSplices,
                                                          defaultHeistState,
                                                          getParamNode)
import           Text.Templating.Heist.TemplateDirectory (TemplateDirectory,
                                                          newTemplateDirectory')

import           Control.Monad.Reader                    (MonadReader,
                                                          ReaderT (..), ask)
import           Happstack.Server                        (BodyPolicy, Browsing (EnableBrowsing),
                                                          Conf (port),
                                                          CookieLife (Session),
                                                          FilterMonad,
                                                          Happstack, HasRqData,
                                                          Method (GET, POST),
                                                          Response, ServerMonad,
                                                          ServerPart,
                                                          ServerPartT,
                                                          WebMonad, addCookie,
                                                          decodeBody,
                                                          defaultBodyPolicy,
                                                          dir, dirs, lookText,
                                                          mapServerPartT,
                                                          method, mkCookie,
                                                          nullConf, ok, path,
                                                          readCookieValue,
                                                          serveDirectory,
                                                          simpleHTTP,
                                                          toResponse)
import           Log
import qualified Text.XmlHtml                            as X

import           Data.Acid                               (AcidState,
                                                          EventResult,
                                                          EventState, IsAcidic,
                                                          Query, QueryEvent,
                                                          Update, UpdateEvent,
                                                          makeAcidic)
import           Data.Acid.Advanced                      (query', update')
import           Data.Acid.Local                         (createCheckpointAndClose,
                                                          openLocalState,
                                                          openLocalStateFrom)
import           Data.Data                               (Typeable)
import           Data.Maybe                              (fromMaybe)

import           Control.Applicative                     (Alternative,
                                                          Applicative, (<$>))
import           Control.Monad.Trans.Control             (MonadBaseControl)

import           BlogTypes
import           Types
import           Blog

-- 工作目录下期待的文件夹: static/ static/tpls/
-- 访问log记录位置 access.log

main :: IO ()
main = do
    withAcid (Just "state") $
        \acid -> simpleHTTP nullConf {port = 80} $ runApp acid handlers

--handlers :: ServerPart Response
handlers :: App Response
handlers = do
    msum [ dir "blog" $ myblog
         , dir "heist" $ myheist
         , dir "cookie" $ mycookie
         , myFiles ]

myFiles :: App Response
myFiles = do
        serveDirectory EnableBrowsing ["index.htm", "index.html"] "static/"

data ServerApp m = ServerApp
    { unHeistState  :: TemplateDirectory m
    , unAcidState   :: AcidState MessageDB
    }

class HasAcidState m st where
   getAcidState :: m (AcidState st)

query :: forall event m. ( Functor m , MonadIO m , QueryEvent event ,
         HasAcidState m (EventState event)) => event -> m (EventResult event)
query event = do
    as <- getAcidState
    query' (as :: AcidState (EventState event)) event


update :: forall event m. ( Functor m , MonadIO m , UpdateEvent event ,
    HasAcidState m (EventState event)) => event -> m (EventResult event)
update event = do
    as <- getAcidState
    update' (as :: AcidState (EventState event)) event

-- automatically creates a checkpoint on close
withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) =>
                  Maybe FilePath -> st -> (AcidState st -> m a) -> m a
withLocalState mPath initialState =
    Lifted.bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (liftIO . createCheckpointAndClose)

-- getGreeting :: Query GreetingState Text
-- getGreeting = _greeting <$> ask
-- $(makeAcidic ''GreetingState ['getGreeting, 'setGreeting])



withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "state" mBasePath
    in withLocalState (Just $ basePath) initMessageDB $ \c ->
           action (Acid c)
       --withLocalState (Just $ basePath </> "greeting") initialGreetingState $ \g ->


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

instance HasAcidState App MessageDB where
    getAcidState = messageDB <$> ask
--instance HasAcidState App GreetingState where
    --getAcidState = acidGreetingState <$> ask

template :: T.Text -> Html -> Response
template title bd = toResponse $
        H.html $ do
          H.head $ do
            H.title (toHtml title)
          H.body $ do
                bd
                p $ a ! href "/" $ "back to home"


-- a demo from happstack tutorial
factSplice :: (Monad m) => HeistT m Template
factSplice = do
    input <- getParamNode
    let text = T.unpack $ X.nodeText input
        n    = read text :: Int
    return [X.TextNode $ T.pack $ show $ product [1..n]]

-- just replace custom tag you to em
mySplice :: (Monad m) => HeistT m Template
mySplice = do
    input <- getParamNode
    return [X.Element (T.pack "em")([])([X.TextNode $ X.nodeText input])]


-- logic free template Jan 04 00:01:51
myheist :: App Response
myheist = do
    td <- liftIO $ newTemplateDirectory' "static/tpls" (bindSplices ss defaultHeistState)
    -- 原来添加默认路径处理的功能是这个函数实现的,nullDir render index
    -- 否则获取路径,然后render
    templateServe td
    -- msum[templateReloader td, templateServe td]
    -- msum[templateServe td, nullDir >> seeOther ("/index"::String) (toResponse ())]
    where
    ss = [(T.pack "me", return [X.Element (T.pack "strong") ([])
                ([X.TextNode $ T.pack "david happy feng"])])
         ,(T.pack "you", mySplice)
         ,(T.pack "fact", factSplice)
         ]

mycookie :: App Response
mycookie =
    msum [ do (requests::Int) <- readCookieValue "requests"
              addCookie Session (mkCookie "requests" (show (requests + 1)))
              ok $ template "cookie" $ do
                        p $ toHtml (("you have made " ++ show requests ++ " requests to this page")::String)
                        p $ "overloaded strings for Html type"
         , do addCookie Session (mkCookie "requests" (show (2:: Int)))
              ok $ template "cookie" $ "this is your first visit"
         ]

openAction :: IO ()
openAction = do
    prepareLog

closeAction :: IO ()
closeAction = return ()
