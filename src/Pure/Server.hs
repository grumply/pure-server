{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings #-}
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
  , ssConnections :: [(Int,WebSocket)]
  }

instance Pure Server where
  view =
      ComponentIO $ \self ->
          let
              handleConnections sock = forever $ do
                  (conn,sockAddr) <- accept sock
                  ws <- serverWS conn
                  u <- hashUnique <$> newUnique
                  onStatus ws $ \status ->
                    case status of
                      Closed _ -> void $ setState self $ \_ st ->
                        return (st { ssConnections = filter ((/= u) . fst) (ssConnections st) }, do
                            return ()
                          )
                      _ -> return ()
                  void $ setState self $ \_ st ->
                    return (st { ssConnections = (u,ws) : ssConnections st }, do
                        return ()
                      )
#ifdef SECURE
              handleSecureConnections ctx sock = forever $ do
                  (conn,sockAddr) <- accept sock
                  ssl <- sslAccept conn
                  ws <- serverWSS conn ssl
                  u <- hashUnique <$> newUnique
                  onStatus ws $ \status ->
                    case status of
                      Closed _ -> void $ setState self $ \_ st ->
                        return (st { ssConnections = filter ((/= u) . fst) (ssConnections st) }, do
                            return ()
                          )
                      _ -> return ()
                  void $ setState self $ \_ st ->
                    return (st { ssConnections = (u,ws) : ssConnections st }, do
                        return ()
                      )
#endif
          in
              def
                  { construct = do
                      s <- getProps self
                      case s of
                        Server {..} -> do
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ void $ handleConnections sock
                          return (ServerState tid sock [])
#ifdef SECURE
                        SecureServer {..} -> do
                          ctx <- sslSetupServer sslKey sslCert sslChain
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleSecureConnections ctx sock
                          return (ServerState tid sock [])
#endif
                  , render = \s ServerState {..} ->
                      Keyed (SimpleHTML "clients") <||#>
                        (fmap (fmap (connection s)) ssConnections)
                  }
#endif
