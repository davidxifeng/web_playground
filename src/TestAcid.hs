{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
 GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
 TypeFamilies, RecordWildCards #-}
module TestAcid where
import Data.Acid    (AcidState, Query, Update, makeAcidic)
import Data.Acid.Local (openLocalState, createCheckpointAndClose)
import Data.Acid.Advanced (query', update')

import Data.SafeCopy (base, deriveSafeCopy)


test :: Int
test = 5

data TestRec = TestRec
    { i :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- 第一次使用模板haskell
$(deriveSafeCopy 0 'base ''TestRec)
-- $()指示这里将使用模板产生的代码,不过这个标记是可选的
-- '用来获取函数名或者构造器的名字
-- ''用来获取类型名或者构造器的名字


