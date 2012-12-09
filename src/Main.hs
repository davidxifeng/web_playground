{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Monad (msum)
import Happstack.Server
--import Happstack.Lite

import Text.Blaze.Html5 (Html(), (!), toHtml, a, p,  input)
import qualified Text.Blaze.Html5 as H

import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.Lazy (unpack)
import Data.Text (Text)

davidConf :: Conf
davidConf = nullConf {port = 80}

main :: IO()
main = simpleHTTP davidConf $ msum
        [ dir "web" $ myFiles
        , dir "echo" $ echo
        , dir "form" $ myform
        , myFiles
        ]


{-
davidConf :: ServerConfig
davidConf = ServerConfig 80 (1 * 10^6) (20 * 10^6) "/tmp/"

main :: IO ()
main = serve (Just davidConf) $ msum
        [ dir "web" myFiles
        , dir "echo" echo
        , dir "form" myform
        , myFiles
        ]
-}
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
