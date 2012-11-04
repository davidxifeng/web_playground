{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Monad (msum)
import Happstack.Server
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding(dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (unpack)
import Data.Text (Text)

davidConf :: Conf
davidConf = nullConf {port = 80}

main :: IO()
main = simpleHTTP davidConf $ msum
	[ dir "web" $ myFiles
	, dir "echo" $ echo
	, myFiles
	]

template :: Text -> Html -> Response
template title body = toResponse $
	H.html $ do
	  H.head $ do
	    H.title (toHtml title)
	  H.body $ do
		body
		p $ a ! href "/" $ "back to home"

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
	ok $ template "echo" $ do
		p $ "echo: " >> toHtml msg
		p "change to see something else"

myFiles :: ServerPart Response
myFiles =
	serveDirectory EnableBrowsing ["index.html"] "static/"
