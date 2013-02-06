{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module AppData where

import           Control.Monad.Reader (MonadReader, ReaderT, ask)
import           Happstack.Server     (FilterMonad, Happstack, HasRqData,
                                       Response, ServerMonad, ServerPartT,
                                       WebMonad)

import           Control.Applicative                     (Alternative,
                                                          Applicative, (<$>))
import           Control.Monad        (MonadPlus)
import           Control.Monad.Trans  (MonadIO)
import           Data.Acid            (AcidState)

import           Text.Templating.Heist.TemplateDirectory (TemplateDirectory)

import           Acid
import           BlogTypes


newtype App a = App
    { unApp :: ServerPartT (ReaderT AppData IO) a
    }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
             , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
             , Happstack, MonadReader AppData
             )

data AppData = AppData
    { messageDB    :: AcidState MessageDB
    , unHeistState  :: TemplateDirectory App
    }

instance HasAcidState App MessageDB where
    getAcidState = messageDB <$> ask
