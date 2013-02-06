{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module BlogTypes where

import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable (..), IxSet, Proxy (..), ixFun,
                                       ixSet, (@=))
import qualified Data.IxSet           as IxSet
import           Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import           Data.Text            (Text)
import           Data.Time            (UTCTime (..))

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)



newtype MessageId = MessageId { unMessageId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

data Message = Message
    { messageId     :: MessageId
    , author    :: Text
    , message      :: Text
    , inTrash      :: Bool
    , timestamp      :: UTCTime
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Message)

newtype Author = Author Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Message where
    empty = ixSet [ ixFun $ \msg -> [messageId msg]
                  , ixFun $ \msg -> [Author $ author msg]
                  , ixFun $ \msg -> [timestamp msg]
                  , ixFun $ \msg -> [inTrash msg]
                  ]

data MessageDB = MessageDB
    { nextMessageId :: MessageId
    , messages      :: IxSet Message
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''MessageDB)

-- | 插入一条新的消息到数据库中
newMessage :: Message -> Update MessageDB ()
newMessage msg = do
    md@MessageDB{..} <- get
    put $ md { nextMessageId = succ nextMessageId
             , messages      = IxSet.insert msg messages
             }

getAllMessages :: Query MessageDB [Message]
getAllMessages = do
    MessageDB{..} <- ask
    return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ messages @= False

getMessageById :: MessageId -> Query MessageDB (Maybe Message)
getMessageById msgId = do
    MessageDB{..} <- ask
    return $ IxSet.getOne $ messages @= msgId

getMessageByAuthor :: Query MessageDB [Message]
getMessageByAuthor = do
    MessageDB{..} <- ask
    return $ IxSet.toList messages

$(makeAcidic ''MessageDB [ 'newMessage
                         , 'getAllMessages
                         , 'getMessageById
                         , 'getMessageByAuthor
                         ])

initMessageDB :: MessageDB
initMessageDB = MessageDB (MessageId 1) IxSet.empty
