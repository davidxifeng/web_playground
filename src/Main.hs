{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Monad
import Control.Monad.Trans
import Happstack.Server

import Text.Blaze.Html5 (Html(), (!), toHtml, a, p,  input)
import qualified Text.Blaze.Html5 as H

import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.Lazy (unpack)
import Data.Text (Text)

import TestAcid

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
main = simpleHTTP davidConf handlers

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
    msum [ do rq <- askRq
              --liftIO $ print (rqPaths rq)
              --this line will failed after stdout not available after
              --logout
              mzero
         , do (requests::Int) <- readCookieValue "requests"
              addCookie Session (mkCookie "requests" (show (requests + 1)))
              ok $ template "cookie" $ do
                        p $ toHtml (("you have made " ++ show requests ++ " requests to this page")::String)
                        p $ "overloaded strings for Html type"
         , do addCookie Session (mkCookie "requests" (show (2:: Int)))
              ok $ template "cookie" $ "this is your first visit"
         ]


template :: Text -> Html -> Response
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
