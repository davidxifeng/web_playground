{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where
import           Control.Exception                       (bracket)
import           Control.Monad                           (msum, mzero)
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
import           Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')

import           Happstack.Server                        (BodyPolicy, Browsing (EnableBrowsing),
                                                          Conf (port),
                                                          CookieLife (Session),
                                                          Method (GET, POST),
                                                          Response, ServerPart,
                                                          addCookie, decodeBody,
                                                          defaultBodyPolicy,
                                                          dir, dirs, lookText,
                                                          method, mkCookie,
                                                          nullConf, ok, path,
                                                          readCookieValue,
                                                          serveDirectory,
                                                          simpleHTTP,
                                                          toResponse)
import qualified Text.XmlHtml                            as X

-- 加入初步的acid-state支持
import           Control.Applicative                     ((<$>))
import           Control.Monad.Reader                    (ask)
import           Control.Monad.State                     (get, put)

import           Log
import           Blog

davidConf :: Conf
davidConf = nullConf {port = 80}

-- 工作目录下期待的文件夹: static/ static/tpls/
-- 访问log记录位置 access.log
main :: IO()
main = do
    bracket (openAction)
            (\_ -> closeAction)
            (\_ -> simpleHTTP davidConf handlers)

openAction :: IO ()
openAction = do
    prepareLog

closeAction :: IO ()
closeAction = return ()


handlers :: ServerPart Response
handlers = do
    msum [ dir "blog" $ myblog
         , dir "heist" $ myheist
         , dir "cookie" $ mycookie
         , myFiles ]


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
myheist :: ServerPart Response
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

mycookie :: ServerPart Response
mycookie =
    msum [ do (requests::Int) <- readCookieValue "requests"
              addCookie Session (mkCookie "requests" (show (requests + 1)))
              ok $ template "cookie" $ do
                        p $ toHtml (("you have made " ++ show requests ++ " requests to this page")::String)
                        p $ "overloaded strings for Html type"
         , do addCookie Session (mkCookie "requests" (show (2:: Int)))
              ok $ template "cookie" $ "this is your first visit"
         ]


myFiles :: ServerPart Response
myFiles = do
        serveDirectory EnableBrowsing ["index.htm", "index.html"] "static/"
