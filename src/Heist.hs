module Heist where
import           AppData
import           Control.Monad.Reader                    (ask)
import qualified Data.Text                               as T
import           Happstack.Server                        (Response)
import           Happstack.Server.Heist                  (templateServe)

import           Text.Templating.Heist                   (HeistT, Template,
                                                          bindSplices,
                                                          defaultHeistState,
                                                          getParamNode)
import           Text.Templating.Heist.TemplateDirectory (TemplateDirectory,
                                                          newTemplateDirectory')
import qualified Text.XmlHtml                            as X


myheist :: App Response
myheist = do
    appData <- ask
    templateServe $ unHeistState appData

loadTpls :: IO (TemplateDirectory App)
loadTpls =
    newTemplateDirectory' "static/tpls" (bindSplices ss defaultHeistState)
    where
    ss = [(T.pack "me", return [ X.Element (T.pack "strong") ([])
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
