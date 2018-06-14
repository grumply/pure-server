{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase #-}
module Pure.Server where

-- from base
import Control.Concurrent
import Control.Monad
import Data.Unique

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from pure-websocket
import Pure.WebSocket

-- from containers
import Data.IntMap as IntMap

data Server
  = Server
    { ip         :: String
    , port       :: Int
    , connection :: WebSocket -> View
    }
#ifdef SECURE
  | SecureServer
    { ip         :: String
    , port       :: Int
    , sslKey     :: FilePath
    , sslCert    :: FilePath
    , sslChain   :: Maybe FilePath
    , connection :: WebSocket -> View
    }
#endif

#ifndef __GHCJS__
data ServerState = ServerState
  { ssListener    :: ThreadId
  , ssSocket      :: Socket
  , ssConnections :: IntMap WebSocket
  }

instance Pure Server where
  view =
      ComponentIO $ \self ->
          let
              updConnections f = modify_ self $ \_ ss -> ss { ssConnections = f (ssConnections ss) }

              handleConnections sock = forever $ do
                  (conn,sockAddr) <- accept sock
                  ws <- serverWS conn
                  u <- hashUnique <$> newUnique
                  onStatus ws $ \case
                    Closed _ -> updConnections (IntMap.delete u)
                    _ -> return ()
                  updConnections (IntMap.insert u ws)
#ifdef SECURE
              handleSecureConnections ctx sock = forever $ do
                  (conn,sockAddr) <- accept sock
                  ssl <- sslAccept conn
                  ws <- serverWSS conn ssl
                  u <- hashUnique <$> newUnique
                  onStatus ws $ \case
                    Closed _ -> updConnections (IntMap.delete u)
                    _        -> return ()
                  updConnections (IntMap.insert u ws)
#endif
          in
              def
                  { construct = do
                      s <- ask self
                      case s of
                        Server {..} -> do
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleConnections sock
                          return (ServerState tid sock IntMap.empty)
#ifdef SECURE
                        SecureServer {..} -> do
                          ctx <- sslSetupServer sslKey sslCert sslChain
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleSecureConnections ctx sock
                          return (ServerState tid sock IntMap.empty)
#endif
                  , render = \s ServerState {..} ->
                      Keyed (SimpleHTML "clients") <||#>
                        (fmap (fmap (connection s)) (IntMap.toAscList ssConnections))
                  }
#endif
