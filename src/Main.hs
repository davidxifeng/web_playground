{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
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
import           Data.Acid                               (AcidState, Query,
                                                          Update, makeAcidic)
                                                          --, query
import           Data.Acid.Advanced                      (query', update')
import           Data.Acid.Local                         (createCheckpointAndClose,
                                                          openLocalState)
import           Data.Data                               (Data, Typeable)
import           Data.IxSet                              (Indexable (..),
                                                          IxSet,
                                                          Proxy (..), getOne,
                                                          ixFun, ixSet, (@=))
import qualified Data.IxSet                              as IxSet
import           Data.SafeCopy                           (SafeCopy, base,
                                                          deriveSafeCopy)

import           System.Log.Formatter                    (simpleLogFormatter)
import           System.Log.Handler                      (setFormatter)
import           System.Log.Handler.Simple               (fileHandler)
-- 默认 >= warning才显示
-- DEBUG INFO NOTICE WARNING ERROR CRITICAL ALERT EMERGENCY
import           System.Log.Logger                       (Priority (..), logM,
                                                          rootLoggerName,
                                                          setHandlers, setLevel,
                                                          updateGlobalLogger)

-- 需要把文件分成多个模块了...
data ViewCount = ViewCount { count :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ViewCount)

aSetter :: Integer ->Update ViewCount Integer
aSetter n = do
    c@ViewCount{..} <- get
    let newCount = count + n
    put $ c { count = newCount }
    return newCount

aGetter :: Query ViewCount Integer
aGetter = count <$> ask

$(makeAcidic ''ViewCount ['aSetter, 'aGetter])

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
closeAction = do
    logM "web server" NOTICE "server is down"


handlers :: ServerPart Response
handlers = do
    msum [ dir "echo" $ echo
         , dir "form" $ myform
         , dir "acid" $ myacid
         , dir "heist" $ myheist
         , dir "cookie" $ mycookie
         , dirs "hi/you" $ ok $ template "test dirs"
            (H.p "just show dirs's usage")
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


myacid :: ServerPart Response
myacid = do
    --just for a quick demo test
    acid <- liftIO $ openLocalState (ViewCount 0)
    --msum [realres acid, (none acid)]
    realres acid
    where
    realres x = do
        -- c <- liftIO $ query x AGetter
        c <- query' x AGetter
        r <- ok $ template "minamal acid demo" $
                H.p $ toHtml (("view " ++ show c :: String))
        _ <- update' x (ASetter 2)
        liftIO $ (createCheckpointAndClose x)
        return r

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

myform :: ServerPart Response
myform = do
    msum [viewForm, processForm]
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
           decodeBody myPolicy
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



echo :: ServerPart Response
echo = msum[ (liftIO $ logM "test" INFO "response an echo") >> mzero
            , echo'
            ]

echo' :: ServerPart Response
echo' = path $ \(msg :: String) ->
        ok $ template "echo" $ do
                p $ "echo: " >> toHtml msg
                p "change to see something else"
                p "测试使用authbind后的post hook"
                p "再次测试,try use exec fix zoombie process"
                p "why?"

myFiles :: ServerPart Response
myFiles = do
        serveDirectory EnableBrowsing ["index.htm", "index.html"] "static/"

-- 目前方案:所有log都放到access.log里. 其实也可以单独为某一类log设定输出目标
prepareLog :: IO ()
prepareLog = do
    h <- fileHandler "access.log" DEBUG
    --logM rootLoggerName WARNING "this should logged to stderr"
    h' <- return $ setFormatter h $
        simpleLogFormatter "[$time : $loggername : $prio] thread id: $tid\n$msg"
    --setHandlers: 用一个列表覆盖原有的
    updateGlobalLogger rootLoggerName (setHandlers [h'] . setLevel INFO)
    logM "web server" NOTICE "server is up"
