{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- to be found MonadThrow

module Control.Monad.Trans.Resource.Internal(
    ExceptionT(..)
  , InvalidAccess(..)
  , MonadResource(..)
  , MonadThrow(..)
  , MonadUnsafeIO(..)
  , ReleaseKey(..)
  , ReleaseMap(..)\
  , ResIO
  , ResourceT(..)
  , stateAlloc
  , stateCleanup
  , transResourceT
) where

class Monad m => MonadThrow m where
    monadThrow :: E.Exception e => e -> m a
