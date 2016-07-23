{-# LANGUAGE FunctionalDependencies #-}

module Graphics.XHB.Requests.Internal.Classes
    ( Request(..)
    , RequestWithReply(..)
    ) where

import Graphics.XHB

class Request a where
    requestIO :: a -> Connection -> IO ()

class RequestWithReply a b | a -> b, b -> a where
    requestWithReplyIO :: a -> Connection -> IO (IO (Either SomeError b))
