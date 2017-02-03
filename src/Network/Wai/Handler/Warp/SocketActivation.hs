{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}

{-|
  Module:      Network.Wai.Handler.Warp.SocketActivation
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description
  A simple wrapper for socket based activation.
-}


module Network.Wai.Handler.Warp.SocketActivation (
    SocketActivationSettings(..)
  , SocketActivationException(..)
  , isSocketActivationError
  , withSocketActivation
  ) where

import Control.Exception.Base (Exception)
import Control.Monad.Catch (MonadMask, bracket, MonadThrow, throwM, handleJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Streaming.Network (bindPortTCP, HostPreference)
import Data.Typeable (Typeable, typeOf)
import Network.Socket (Socket, fdSocket, close)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import System.Posix.Internals (setNonBlockingFD)
import System.Systemd.Daemon (getActivatedSockets)


-- | The exception is thrown when something goes wrong with this package.
data SocketActivationException
     = NoSocketsActivated
     -- ^ Thrown when the application is started without systemd
     -- socket and no fallback port is specified.

     | MultipleSocketsActivated Int
     -- ^ Thrown when more than one systemd socket activated the
     -- application.

    deriving (Eq, Show, Typeable)

instance (Exception SocketActivationException)

-- | Exception filter.
isSocketActivationError :: (Exception e) => e -> Bool
isSocketActivationError = (typeOf (undefined :: SocketActivationException) ==) . typeOf


-- | Options that determine activation mechanism.
data SocketActivationSettings a = SocketActivationSettings {
    sasPort :: Maybe Int
    -- ^ Fallback port to use when the application is started without systemd socket.

  , sasHostPreference :: HostPreference
    -- ^ Fallback host preference.

  , sasFallbackAction :: SocketActivationException -> IO a
    -- ^ When no sockets are activated, execute action (if specified) or throw exception.
  }

instance Default (SocketActivationSettings a) where
  def = SocketActivationSettings {
      sasPort = Nothing
    , sasHostPreference = "*"
    , sasFallbackAction = throwM
    }


-- | Wrapper for socket-activated function.
withSocketActivation :: forall m a. (MonadIO m, MonadMask m)
  => SocketActivationSettings a
  -> (Socket -> m a)
  -> m a
withSocketActivation set f = handleJust
  (\e -> if isSocketActivationError e then Just e else Nothing)
  (liftIO . sasFallbackAction set)
  run where
    run = getSocket set >>= \socket -> bracket (return socket) (liftIO . close) f


getSocket :: forall m a. (MonadIO m, MonadThrow m)
  => SocketActivationSettings a
  -> m Socket
getSocket (SocketActivationSettings {..}) = (liftIO getActivatedSockets) >>= \case
  Nothing -> ($ sasPort) $ maybe
    (throwM NoSocketsActivated)
    (\port -> liftIO $ do
        sock <- bindPortTCP port sasHostPreference
        setFdOption (fromIntegral $ fdSocket sock) CloseOnExec True
        return sock)

  Just [sock] -> liftIO $ do
    setNonBlockingFD (fdSocket sock) True
    return sock

  Just ss -> throwM $ MultipleSocketsActivated (length ss)

