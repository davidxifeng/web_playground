module Log
    (prepareLog)
    where

import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)

-- 默认 >= warning才显示
-- DEBUG INFO NOTICE WARNING ERROR CRITICAL ALERT EMERGENCY
import           System.Log.Logger         (Priority (..), logM, rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)

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

