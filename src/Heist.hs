module Heist where
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
import qualified Text.XmlHtml                            as X
import           Control.Monad.Reader                    (MonadReader,
                                                          ReaderT (..), ask)
import AppData


loadTpls :: IO (TemplateDirectory App)
loadTpls =
    newTemplateDirectory' "static/tpls" (bindSplices ss defaultHeistState)
    where
    ss = [(T.pack "me", return [X.Element (T.pack "strong") ([])
                ([X.TextNode $ T.pack "david happy feng"])])
         ,(T.pack "you", mySplice)
         ,(T.pack "fact", factSplice)
         ] 
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
    -- 原来添加默认路径处理的功能是这个函数实现的,nullDir render index
    -- 否则获取路径,然后render
    appData <- ask
    templateServe $ unHeistState appData
    -- msum[templateReloader td, templateServe td]
    -- msum[templateServe td, nullDir >> seeOther ("/index"::String) (toResponse ())]
