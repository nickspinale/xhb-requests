{-# LANGUAGE FunctionalDependencies #-}

module Graphics.XHB.Monad.Internal.Classes
    ( Request(..)
    , Notice(..)
    ) where

import Graphics.XHB

class Request a b | a -> b where
    request :: Connection -> a -> IO (IO (Either SomeError b))

class Notice a where
    notice :: Connection -> a -> IO ()
