{-# LANGUAGE FunctionalDependencies #-}

module Graphics.XHB.Requests.Internal.Classes
    ( Request(..)
    , RequestWithReply(..)
    ) where

import Graphics.XHB

class Request a where
    requestIO :: Connection -> a -> IO ()

class RequestWithReply a b | a -> b, b -> a where
    requestWithReplyIO :: Connection -> a -> IO (IO (Either SomeError b))
