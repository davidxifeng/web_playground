{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Types where


import           Control.Monad.Reader                    (MonadReader, ReaderT (..))
import           Happstack.Server                        (FilterMonad,
                                                          Happstack, HasRqData,
                                                          Response, ServerMonad,
                                                          ServerPartT,
                                                          WebMonad)

import           Data.Acid                               (AcidState)
import           Control.Applicative                     (Alternative, Applicative)
import           Control.Monad                           (MonadPlus)
import           Control.Monad.Trans                     (MonadIO)
-- import           Control.Monad.Trans.Control             (MonadBaseControl)

import BlogTypes


data Acid = Acid { messageDB    :: AcidState MessageDB
                 }


newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)
