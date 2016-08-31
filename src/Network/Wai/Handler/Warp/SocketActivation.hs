{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

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
  , withSocketActivation
  , withSocketActivationM
  ) where

import Control.Exception.Base (Exception, bracket)
import Control.Monad.Catch (MonadThrow (..), catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Streaming.Network (bindPortTCP, HostPreference)
import Data.Typeable (Typeable)
import Network.Socket (Socket, fdSocket, close)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import System.Posix.Internals (setNonBlockingFD)
import System.Systemd.Daemon (getActivatedSockets)
  

-- | Options that determine activation mechanism.
data SocketActivationSettings = SocketActivationSettings {
    sasPort :: Maybe Int 
    -- ^ Fallback port to use when the application is started without systemd socket.

  , sasHostPreference :: HostPreference
    -- ^ Fallback host preference.
  }

instance Default SocketActivationSettings where
  def = SocketActivationSettings {
      sasPort = Nothing
    , sasHostPreference = "*"
    }

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

-- | Wrapper for socket-activated function.
withSocketActivation :: SocketActivationSettings -> (Socket -> IO a) -> IO a
withSocketActivation set f = catch (withSocketActivationM set f) $
  \(ex :: SocketActivationException) -> error $ show ex


-- | Wrapper for socket-activated function.
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
