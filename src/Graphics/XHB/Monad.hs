module Graphics.XHB.Monad
    ( Request(..)
    , Notice(..)
    , X
    , req
    , reqAsync
    , notify
    , IOU(..)
    , (<$>>)
    , (<*>>)
    ) where


import Graphics.XHB
import Graphics.XHB.Monad.Internal.Classes
import Graphics.XHB.Monad.Internal.Instances ()
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


newtype X m a = X { runX :: ReaderT Connection (ExceptT SomeError m) a }

instance MonadTrans X where

instance Functor m => Functor (X m) where

instance Applicative m => Applicative (X m) where

instance Monad m => Monad (X m) where


req :: (Request a b, MonadIO m) => a -> X m b
req a = X . ReaderT $ \conn -> ExceptT . liftIO . join $ request conn a

reqAsync :: (Request a b, MonadIO m) => a -> X m (X m b)
reqAsync a = X . ReaderT $ \conn -> liftIO . fmap (X . lift . ExceptT . liftIO) $ request conn a

notify :: (Notice a, MonadIO m) => a -> X m ()
notify a = X . ReaderT $ \conn -> liftIO (notice conn a)


newtype IOU m a = IOU { runIOU :: m (m a) }

instance Functor m => Functor (IOU m) where
    fmap f = IOU . fmap (fmap f) . runIOU

instance Monad m => Applicative (IOU m) where
    pure = IOU . pure . pure
    (IOU ma) <*> (IOU mb) = IOU $ (<*>) <$> ma <*> mb

(<$>>) :: (Request a b, MonadIO m) => (b -> c) -> a -> X m (X m c)
f <$>> a = runIOU . fmap f . IOU $ reqAsync a

(<*>>) :: (Request a b, MonadIO m) => X m (X m (b -> c)) -> a -> X m (X m c)
f <*>> a = runIOU $ IOU f <*> (IOU (reqAsync a))
