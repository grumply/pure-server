{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables #-}
module Pure.Server where

-- from base
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Unique

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from pure-websocket
import Pure.WebSocket hiding (handle)

-- from containers
import Data.IntMap as IntMap

data Server
  = Server_
    { ip         :: String
    , port       :: Int
    , connection :: WebSocket -> View
    }
#ifdef SECURE
  | SecureServer_
    { ip         :: String
    , port       :: Int
    , sslKey     :: FilePath
    , sslCert    :: FilePath
    , sslChain   :: Maybe FilePath
    , connection :: WebSocket -> View
    }

pattern SecureServer :: String -> Int -> FilePath -> FilePath -> Maybe FilePath -> (WebSocket -> View) -> View
pattern SecureServer ip port sslKey sslCert sslChain connection = View (SecureServer_ ip port sslKey sslCert sslChain connection)
#endif

#ifndef __GHCJS__

pattern Server :: String -> Int -> (WebSocket -> View) -> View
pattern Server ip port connection = View (Server_ ip port connection)

data ServerState = ServerState
  { ssListener    :: ThreadId
  , ssSocket      :: Socket
  , ssConnections :: !(IntMap WebSocket)
  }

instance Pure Server where
  view =
      ComponentIO $ \self ->
          let
              updConnections f = modify_ self $ \_ ss -> ss { ssConnections = f (ssConnections ss) }

              handleConnections sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
                  (conn,sockAddr) <- accept sock
                  void $ forkIO $ do
                    ws <- serverWS conn
                    u <- hashUnique <$> newUnique
                    onStatus ws $ \case
                      Closed _ -> updConnections (IntMap.delete u)
                      _ -> return ()
                    updConnections (IntMap.insert u ws)
#ifdef SECURE
              handleSecureConnections ctx sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
                  (conn,sockAddr) <- accept sock
                  void $ forkIO $ do
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
                        Server_ {..} -> do
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleConnections sock
                          return (ServerState tid sock IntMap.empty)
#ifdef SECURE
                        SecureServer_ {..} -> do
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
