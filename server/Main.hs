{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Main where

import           Control.Applicative           (pure, (<|>))
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Lens.Getter           ((^.))
import           Control.Lens.Lens             (Lens', lens, (&))
import           Control.Lens.Setter           ((%~), (.~))
import           Control.Monad                 (forever, unless)

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Foldable                 (sequence_)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UID
import           Data.UUID.V4

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Network.WebSockets.Snap

import           Prelude                       hiding (return)

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe


{- Data/type definitions -}
data Client = Client
    { _uuid       :: UUID
    , _username   :: ByteString
    , _connection :: WS.Connection
    , _currGame   :: ByteString
    }

instance Eq Client where
    (==) c1 c2 = c1^.uuid == c2^.uuid

instance Ord Client where
    compare c1 c2 = compare (c1^.uuid) (c2^.uuid)

instance Show Client where
    show c = "Client {"
          ++ UID.toString (c^.uuid)
          ++ ", "
          ++ BC.unpack (c^.username)
          ++ "}"

type GameState = Int

data Game = Game
    { _gameName  :: ByteString
    , _players   :: Set Client
    , _gameState :: GameState
    } deriving (Show)

data ServerState = ServerState
    { _clients :: Map UUID Client
    , _games   :: Map ByteString Game
    } deriving (Show)


{- Lenses -}
clients :: Lens' ServerState (Map UUID Client)
clients = lens _clients (\s cs -> s { _clients = cs })

games :: Lens' ServerState (Map ByteString Game)
games = lens _games (\s gs -> s { _games = gs })

gameName :: Lens' Game ByteString
gameName = lens _gameName (\g gn -> g { _gameName = gn })

players :: Lens' Game (Set Client)
players = lens _players (\g ps -> g { _players = ps })

gameState :: Lens' Game GameState
gameState = lens _gameState (\g gs -> g { _gameState = gs })

uuid :: Lens' Client UUID
uuid = lens _uuid (\c uu -> c { _uuid = uu })

username :: Lens' Client ByteString
username = lens _username (\c un -> c { _username = un })

connection :: Lens' Client WS.Connection
connection = lens _connection (\c cn -> c { _connection = cn })

currGame :: Lens' Client ByteString
currGame = lens _currGame (\c cg -> c { _currGame = cg })

{- Constants -}
initServerState :: ServerState
initServerState = ServerState { _clients = Map.empty, _games = Map.empty }

hello :: ByteString
hello = B.pack [0x05, 0x58, 0xe6, 0x39, 0x0a, 0xc4, 0x13, 0x24]

{- Utility functions -}
infixr 5 *+
(*+) :: Monoid a => a -> a -> a
(*+) = mappend

uuidToReadableBs :: UUID -> ByteString
uuidToReadableBs = BC.pack . UID.toString
{-# INLINE uuidToReadableBs #-}

newClient :: UUID -> WS.Connection -> Client
newClient uid conn = Client
    { _uuid       = uid
    , _connection = conn
    , _username   = ""
    , _currGame   = ""
    }
{-# INLINE newClient #-}

newGame :: ByteString -> Client -> Game
newGame name host = Game
    { _gameName  = name
    , _players   = Set.singleton host
    , _gameState = 0
    }
{-# INLINE newGame #-}

numClients :: ServerState -> Int
numClients server = Map.size $! server^.clients
{-# INLINE numClients #-}

clientExists :: Client -> ServerState -> Bool
clientExists client server = (client^.uuid) `Map.member` (server^.clients)
{-# INLINE clientExists #-}

addClient :: Client -> ServerState -> ServerState
addClient client server =
    server & clients %~ Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeFromGame :: Client -> ServerState -> ServerState
removeFromGame client server =
    if B.null (client^.currGame) then
        server
    else
        -- Lens magic that deletes the client from the game
        -- that they are `curr`ently in.
        server & games %~
            Map.adjust (& players %~ Set.delete client) (client^.currGame)
{-# INLINE removeFromGame #-}

unregisterClient :: Client -> ServerState -> ServerState
unregisterClient client server = server & clients %~ Map.delete (client^.uuid)
{-# INLINE unregisterClient #-}

removeClient :: Client -> ServerState -> ServerState
removeClient client = unregisterClient client . removeFromGame client
{-# INLINE removeClient #-}

createNewGame :: Client -> ByteString -> ServerState -> ServerState
createNewGame host name =
    (& games %~ Map.insert name (newGame name host)) . (addClient host)
{-# INLINE createNewGame #-}

parseByteString' :: [ByteString] -> ByteString -> [ByteString]
parseByteString' rs s =
    if B.null s then
        rs
    else
        let (r, t) = B.splitAt (fromIntegral (B.head s)) (B.tail s)
        in  parseByteString' (r : rs) t

parseByteString :: ByteString -> [ByteString]
parseByteString s = reverse (parseByteString' [] s)
{-# INLINE parseByteString #-}

{- Primary functions -}
broadcast :: ByteString -> ServerState -> IO ()
broadcast msg server = sequence_ $!
    ((`WS.sendBinaryData` msg) . (^. connection)) <$> (server^.clients)

sendGameList :: ServerState -> Client -> IO ()
sendGameList state client =
    let conn = client^.connection
        getGameInfo g = B.cons (fromIntegral (B.length $! g^.gameName))
                      $! g^.gameName
                     *+ B.singleton (fromIntegral $! Set.size $! g^.players)
        gameInfoList = getGameInfo <$> Map.elems (state^.games)
    in  WS.sendBinaryData conn (B.cons 0 $! B.concat gameInfoList)

registerNewGame :: TVar ServerState -> Client -> ByteString -> IO ()
registerNewGame tState host msg =
    if length parsed /= 2 || any invalidString parsed then
        WS.sendBinaryData conn ("\1\1" :: ByteString)
    else let [newUsername, newGameName] = parsed in
        if | B.length newUsername > 24 ->
                WS.sendBinaryData conn ("\1\2" :: ByteString)
           | B.length newGameName > 32 ->
                WS.sendBinaryData conn ("\1\3" :: ByteString)
           | otherwise -> do
                let sameUsername client = client^.username == newUsername
                state <- readTVarIO tState
                if any sameUsername $! Map.elems (state^.clients) then
                    WS.sendBinaryData conn ("\1\2" :: ByteString)
                else if newGameName `Map.member` (state^.games) then
                    WS.sendBinaryData conn ("\1\3" :: ByteString)
                else do
                    let newHost = host & username .~ newUsername
                                       & currGame .~ newGameName
                    atomically $!
                        modifyTVar' tState (createNewGame newHost newGameName)
                    WS.sendBinaryData conn ("\1\0" :: ByteString)
  where
    conn = host^.connection
    parsed = parseByteString msg
    invalidString s = B.length s < 2 || B.any (\w -> w < 32 || w > 126) s

commLoop :: WS.Connection -> TVar ServerState -> Client -> IO ()
commLoop conn tState client = forever $! do
    msg <- WS.receiveData conn
    state <- readTVarIO tState
    unless (B.null msg) $! case B.head msg of
        0 -> sendGameList state client
        1 -> registerNewGame tState client (B.tail msg)
        _ -> pure ()

webSocketConnect :: TVar ServerState -> WS.ServerApp
webSocketConnect state pending = do
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn

    if msg /= hello then
        WS.sendCloseCode conn 1002 ("Wrong announcement" :: ByteString)
    else do
        WS.forkPingThread conn 30
        newUuid <- nextRandom
        WS.sendBinaryData conn $! UID.toByteString newUuid
        print newUuid

        let client = newClient newUuid conn
        let disconnect = atomically $! modifyTVar' state (removeClient client)

        flip finally disconnect $! do
            atomically $! modifyTVar' state $! addClient client
            commLoop conn state client

site :: TVar ServerState -> Snap ()
site state =
        ifTop (serveFile "public/index.html")
    <|> route [ ("ws", runWebSocketsSnap $! webSocketConnect state)
              , ("",   serveDirectory "public")
              ]

main :: IO ()
main = do
    state <- atomically $! newTVar initServerState
    httpServe (setPort 3000 mempty) (site state)
