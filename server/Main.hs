{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative           (pure)
import           Control.Arrow                 (second)
import           Control.Concurrent.MVar
import           Control.Exception             (finally)
import           Control.Monad                 (forever)

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.ByteString.Lazy          (toStrict)
import           Data.Foldable                 (sequence_)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.UUID                     (UUID, toByteString)
import           Data.UUID.V4

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Network.WebSockets.Snap

import           Prelude                       hiding (return)

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe


{- Data/type definitions -}
data Client = Client {-# UNPACK #-} !UUID {-# UNPACK #-} !WS.Connection

type ServerState = Map UUID WS.Connection


{- Constants -}
initServerState :: ServerState
initServerState = Map.empty

hello :: ByteString
hello = B.pack [0x05, 0x58, 0xe6, 0x39, 0x0a, 0xc4, 0x13, 0x24]

{- Utility functions -}
infixr 5 *+
(*+) :: Monoid a => a -> a -> a
(*+) = mappend

showState :: ServerState -> String
showState m = show $ second (const ("conn" :: String)) <$> Map.toList m

uuidToBs :: UUID -> ByteString
uuidToBs = toStrict . toByteString

uuidOf :: Client -> UUID
uuidOf (Client uuid _) = uuid

connOf :: Client -> WS.Connection
connOf (Client _ conn) = conn

numClients :: ServerState -> Int
numClients = Map.size

clientExists :: Client -> ServerState -> Bool
clientExists (Client uuid _) = Map.member uuid

addClient :: Client -> ServerState -> ServerState
addClient (Client uuid conn) = Map.insert uuid conn

removeClient :: Client -> ServerState -> ServerState
removeClient (Client uuid _) = Map.delete uuid

{- Primary functions -}
broadcast :: ByteString -> ServerState -> IO ()
broadcast msg = sequence_ . fmap (`WS.sendBinaryData` msg)

commLoop :: WS.Connection -> MVar ServerState -> Client -> IO ()
commLoop conn state (Client uuid _) = forever $ do
    msg <- WS.receiveData conn
    print msg
    readMVar state >>= broadcast (uuidToBs uuid *+ " | " *+ msg)

webSocketConnect :: MVar ServerState -> WS.ServerApp
webSocketConnect state pending = do
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn

    if msg /= hello then
        WS.sendCloseCode conn 1002 ("Wrong announcement" :: ByteString)
    else do
        WS.forkPingThread conn 30
        uuid <- nextRandom
        WS.sendBinaryData conn $ toByteString uuid
        print uuid
        -- WS.sendClose conn ("Connection closed normally" :: ByteString)

        -- clients <- readMVar state
        let client = Client uuid conn
        let disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in pure (s', s')
                broadcast (uuidToBs (uuidOf client) *+ " disconnected") s

        flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
                let s' = addClient client s
                WS.sendBinaryData conn $
                    "Welcome! Users: " *+
                        B.intercalate ", " (uuidToBs <$> Map.keys s')
                broadcast (uuidToBs (uuidOf client) *+ " joined") s'
                pure s'
            commLoop conn state client
        {-
        if | not (prefix `T.isPrefixOf` msg) ->
                WS.sendTextData conn ("Wrong announcement" :: Text)
           | any ($ fst client)
                   [T.null, T.any isPunctuation, T.any isSpace] ->
                       WS.sendTextData conn ("Name cannot contain punctuation or " *+
                           "whitespace, and cannot be empty" :: Text)
           | clientExists client clients ->
                   WS.sendTextData conn ("User already exists" :: Text)
           | otherwise -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $
                        "Welcome! Users: " *+ T.intercalate ", " (fst <$> s)
                    broadcast (fst client *+ " joined") s'
                    pure s'
                talk conn state client
        -}

site :: MVar ServerState -> Snap ()
site state =
    route [ ("ws", runWebSocketsSnap $ webSocketConnect state)
          , ("",   serveDirectory "public")
          ]

main :: IO ()
main = do
    state <- newMVar initServerState
    httpServe (setPort 3000 mempty) (site state)
