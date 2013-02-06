{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where
import           Control.Exception (bracket)
import           Control.Monad                           (MonadPlus, mplus,
                                                          msum, mzero)
import qualified Data.Text                               as T
import           Text.Blaze.Html5                        (Html (), a, p, toHtml,
                                                          (!))
import qualified Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             (href)

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
                                                          ServerPartT, WebMonad,
                                                          addCookie, decodeBody,
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


import           Data.Acid                               (AcidState,
                                                          EventResult,
                                                          EventState, IsAcidic,
                                                          Query, QueryEvent,
                                                          Update, UpdateEvent,
                                                          makeAcidic)
import           Data.Acid.Local                         (createCheckpointAndClose,
                                                          openLocalState,
                                                          openLocalStateFrom)

import           Control.Applicative                     (Alternative,
                                                          Applicative, (<$>))

import           Blog
import           BlogTypes
import           AppData
import           Heist

-- 工作目录下期待的文件夹: static/ static/tpls/
-- 访问log记录位置 access.log

main :: IO ()
main = do
    bracket (openAction)
            (\ appData -> closeAction appData)
            (\ appData -> simpleHTTP nullConf {port = 80} $ runApp appData handlers)


openAction :: IO AppData
openAction = do
    prepareLog
    r <- openLocalState initMessageDB
    td <- loadTpls
    let appData = AppData r td
    return appData

closeAction :: AppData -> IO ()
closeAction appData = do
    createCheckpointAndClose $ messageDB appData

runApp :: AppData -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

handlers :: App Response
handlers =
    msum [ dir "blog" myblog
         , dir "heist" myheist
         , dir "cookie" mycookie
         , myFiles ]

myFiles :: App Response
myFiles = serveDirectory EnableBrowsing ["index.htm", "index.html"] "static/"

template :: T.Text -> Html -> Response
template title bd = toResponse $
        H.html $ do
          H.head $ do
            H.title (toHtml title)
          H.body $ do
                bd
                p $ a ! href "/" $ "back to home"


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
