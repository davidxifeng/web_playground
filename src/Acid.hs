{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Acid where

import           Control.Monad.Trans (MonadIO)

import           Data.Acid           (AcidState, EventResult, EventState,
                                      QueryEvent, UpdateEvent)
import           Data.Acid.Advanced  (query', update')



class HasAcidState m st where
   getAcidState :: m (AcidState st)

query :: forall event m. ( Functor m , MonadIO m , QueryEvent event ,
         HasAcidState m (EventState event)) => event -> m (EventResult event)
query event = do
    as <- getAcidState
    query' (as :: AcidState (EventState event)) event


update :: forall event m. ( Functor m , MonadIO m , UpdateEvent event ,
    HasAcidState m (EventState event)) => event -> m (EventResult event)
update event = do
    as <- getAcidState
    update' (as :: AcidState (EventState event)) event


