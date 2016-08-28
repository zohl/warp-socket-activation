{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.SocketActivation (
    SocketActivationSettings(..)
  , SocketActivationException(..)
  , withSocketActivation
  , withSocketActivationM
  ) where

import Control.Exception.Base (Exception, bracket)
import Control.Monad.Catch (MonadThrow (..), catch, throwM)
import Control.Monad.IO.Class
import Data.Default (Default, def)
import Data.Streaming.Network (bindPortTCP, HostPreference)
import Data.Typeable (Typeable)
import Network.Socket (Socket, fdSocket, close)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import System.Posix.Internals (setNonBlockingFD)
import System.Systemd.Daemon (getActivatedSockets)

  
data SocketActivationSettings = SocketActivationSettings {
    sasPort :: Maybe Int 
  , sasHostPreference :: HostPreference
  }

instance Default SocketActivationSettings where
  def = SocketActivationSettings {
      sasPort = Nothing
    , sasHostPreference = "*"
    }

data SocketActivationException
     = NoSocketsActivated
     | MultipleSocketsActivated Int

    deriving (Eq, Show, Typeable)

instance (Exception SocketActivationException)


withSocketActivation :: SocketActivationSettings -> (Socket -> IO a) -> IO a
withSocketActivation set f = catch (withSocketActivationM set f) $
  \(ex :: SocketActivationException) -> error $ show ex


withSocketActivationM :: (MonadIO m, MonadThrow m)
  => SocketActivationSettings
  -> (Socket -> IO a)
  -> m a
withSocketActivationM set f = getSocket set >>= \socket -> liftIO $ bracket (return socket) close f


getSocket :: (MonadIO m, MonadThrow m)
  => SocketActivationSettings
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
