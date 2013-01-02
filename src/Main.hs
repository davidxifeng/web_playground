{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Monad
import Happstack.Server
import Happstack.Server.Heist (templateServe)
import Text.Blaze.Html5 (Html(), (!), toHtml, a, p)--input
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Control.Monad.Trans    (MonadIO, liftIO)
import Text.Templating.Heist  (HeistT, Template,
        bindSplices, defaultHeistState, getParamNode)
import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')

import qualified Text.XmlHtml as X

-- 工作目录下期待的文件夹: static/ static/tpls/
main :: IO()
main = do
    simpleHTTP davidConf handlers

davidConf :: Conf
davidConf = nullConf {port = 80}

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers = do
        decodeBody myPolicy
        msum [ dir "echo" $ echo'
             , dir "form" $ myform
             , dir "heist" $ myheist
             , dir "cookie" $ mycookie
             , dirs "hi/you" $ ok "dirs can use as shorthand"
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


myheist :: ServerPart Response
myheist = do
    td <- liftIO $ newTemplateDirectory' "static/tpls" (bindSplices ss defaultHeistState)
    templateServe td
    -- msum [templateReloader td, templateServe td]
    -- 会自动刷新,与文档不同,晕
    -- msum[templateServe td, nullDir >> seeOther ("/index"::String) (toResponse ())]
    -- 无需重定向
    where
    ss = [(T.pack "me", return [X.Element (T.pack "strong" ) ([])
                ([X.TextNode $ T.pack "david happy feng"])])
         ,(T.pack "you", mySplice)
         ,(T.pack "fact", factSplice)
         ]

myform :: ServerPart Response
myform = msum [viewForm, processForm]
    where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ template "form" $
               H.form ! A.action "/form" ! A.enctype "multipart/form-data" !  A.method "POST" $ do
                        H.label ! A.for "msg" $ "say you"
                        H.input ! A.type_ "text" ! A.id "msg" ! A.name "msg"
                        H.input ! A.type_ "submit" ! A.value "say it"
    processForm :: ServerPart Response
    processForm =
        do method POST
           msg <- lookText "msg"
           ok $ template "form" $ do
                H.p "you said:"
                H.p (toHtml msg)


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



echo' :: ServerPart Response
echo' = msum[do
                 --liftIO $ putStrLn "hi,jim"
                 mzero
            ,echo
            ]

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
        ok $ template "echo" $ do
                p $ "echo: " >> toHtml msg
                p "change to see something else"

myFiles :: ServerPart Response
myFiles = do
        serveDirectory EnableBrowsing ["index.html"] "static/"
