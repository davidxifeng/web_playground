{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Types where

import           Control.Monad.Reader (MonadReader, ReaderT, ask)
import           Happstack.Server     (FilterMonad, Happstack, HasRqData,
                                       Response, ServerMonad, ServerPartT,
                                       WebMonad)

import           Control.Applicative                     (Alternative,
                                                          Applicative, (<$>))
import           Control.Monad        (MonadPlus)
import           Control.Monad.Trans  (MonadIO)
import           Data.Acid            (AcidState)

import           Acid
import           BlogTypes


newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

data Acid = Acid { messageDB    :: AcidState MessageDB
                 }

instance HasAcidState App MessageDB where
    getAcidState = messageDB <$> ask
