{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Monad
-- import Control.Monad.Trans
import Happstack.Server

import Text.Blaze.Html5 (Html(), (!), toHtml, a, p)--input
import qualified Text.Blaze.Html5 as H

import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5.Attributes as A


--import Data.Text.Lazy (unpack)

-- temp for heist test
--import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans    (MonadIO)

import Happstack.Server.Heist (templateReloader, templateServe, render)
import Text.Templating.Heist  (HeistT, Template, HeistState,
        bindSplice, bindSplices, defaultHeistState, getParamNode)
import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')

import qualified Text.XmlHtml as X


factSplice :: (Monad m) => HeistT m Template
factSplice = do
  input <- getParamNode
  let text = T.unpack $ X.nodeText input
      n    = read text :: Int
  return [X.TextNode $ T.pack $ show $ product [1..n]]

templateState :: (MonadIO m) => HeistState m
templateState = bindSplice (T.pack "fact") factSplice defaultHeistState

mySplice1 :: (Monad m) => HeistT m Template
mySplice1 = do
  return [X.TextNode $ T.pack "david happy feng"]

mySplice2 :: (Monad m) => HeistT m Template
mySplice2 = do
  _ <- getParamNode
  return [X.TextNode $ T.pack "I am david"]

mySplice3 :: (Monad m) => HeistT m Template
mySplice3 = do
  --input <- getParamNode
  return [X.Element (T.pack "strong")([])([X.TextNode $ T.pack "I am a text node"])]


davidConf :: Conf
davidConf = nullConf {port = 80}

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers = do
        decodeBody myPolicy
        msum [ dir "web" $ myFiles
             , dir "echo" $ echo
             , dir "form" $ myform
             , dir "cookie" $ mycookie
             , myFiles ]


main :: IO()
main = do
    td <- newTemplateDirectory' "." (templateState)
    td2 <- newTemplateDirectory' "." (bindSplices ss defaultHeistState)
    simpleHTTP davidConf $ msum 
        [ dir "jack" $ templateServe td
        , dir "j2" $ templateServe td2
        --, dir "reload" $ nullDir >> templateReloader td
        --, nullDir >> seeOther ("/factorial":: String) (toResponse ())
        ]
    --simpleHTTP davidConf handlers
    where
    ss = [(T.pack "who", mySplice1)
         ,(T.pack "me", mySplice2)
         ,(T.pack "you", mySplice3)
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
    msum [ -- do rq <- askRq
              --liftIO $ print (rqPaths rq)
              --this line will failed after stdout not available after
              --logout
              --mzero ,
           do (requests::Int) <- readCookieValue "requests"
              addCookie Session (mkCookie "requests" (show (requests + 1)))
              ok $ template "cookie" $ do
                        p $ toHtml (("you have made " ++ show requests ++ " requests to this page")::String)
                        p $ "overloaded strings for Html type"
         , do addCookie Session (mkCookie "requests" (show (2:: Int)))
              ok $ template "cookie" $ "this is your first visit"
         ]


template :: T.Text -> Html -> Response
template title bd = toResponse $
        H.html $ do
          H.head $ do
            H.title (toHtml title)
          H.body $ do
                bd
                p $ a ! href "/" $ "back to home"

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
        ok $ template "echo" $ do
                p $ "echo: " >> toHtml msg
                p "change to see something else"

myFiles :: ServerPart Response
myFiles =
        serveDirectory EnableBrowsing ["index.html"] "static/"
