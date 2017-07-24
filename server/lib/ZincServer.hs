{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module      : ZincServer
--   Description : Server for the /zinc/ game.
--   Copyright   : [copyleft] AugmentedFifth, 2017
--   License     : AGPL-3
--   Maintainer  : zcomito@gmail.com
--   Stability   : experimental
--   Portability : POSIX
module ZincServer
    ( -- * Data/type definitions
      Client
    , GameState
    , Game
    , ServerState
    , -- * Constants
      initServerState
    , hello
    , -- * Utility functions
      (*+)
    , newClient
    , newGame
    , numClients
    , clientExists
    , addClient
    , removeFromGame
    , unregisterClient
    , removeClient
    , removeClientByUuid
    , createNewGame
    , parseByteString
    , -- * Primary functions
      broadcast
    , sendGameList
    , registerNewGame
    , commLoop
    , webSocketConnect
    , site
    , main
    ) where

import           Control.Applicative           (pure, (<|>))
import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Exception             (finally, throwIO)
import           Control.Lens.Getter           ((^.))
import           Control.Lens.Lens             (Lens', lens, (&))
import           Control.Lens.Setter           (ASetter, (%~), (.~))
import           Control.Monad                 (forever, unless)

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Foldable                 (sequence_)
import           Data.Map.Strict               (Map, (!?))
import qualified Data.Map.Strict               as Map
import           Data.Sequence                 (Seq, (|>))
import qualified Data.Sequence                 as Seq
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UID
import           Data.UUID.V4
import           Data.Word                     (Word8)

import           Linear.Metric
import           Linear.V2                     (V2 (V2))
import           Linear.Vector

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Network.WebSockets.Snap

import           Prelude                       hiding (return)

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe

import           System.IO.Error               (doesNotExistErrorType,
                                                mkIOError)


-- * Data/type definitions

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

type Key = V2 Double

data Input = Input
    { _timeStamp :: Double
    , _key       :: Key
    , _isDown    :: Bool
    } deriving (Show)

data Color = Color Word8 Word8 Word8
    deriving (Eq, Show)

data PlayerState = PlayerState
    { _pos        :: V2 Double
    , _vel        :: V2 Double
    , _inputQueue :: Seq Input
    , _color      :: Color
    } deriving (Show)

type GameState = Int

data Game = Game
    { _gameName  :: ByteString
    , _players   :: Map Client PlayerState
    , _gameState :: GameState
    } deriving (Show)

data ServerState = ServerState
    { _clients :: Map UUID Client
    , _games   :: Map ByteString Game
    } deriving (Show)


{- Lenses -}
uuid :: Lens' Client UUID
uuid = lens _uuid (\c uu -> c { _uuid = uu })

username :: Lens' Client ByteString
username = lens _username (\c un -> c { _username = un })

connection :: Lens' Client WS.Connection
connection = lens _connection (\c cn -> c { _connection = cn })

currGame :: Lens' Client ByteString
currGame = lens _currGame (\c cg -> c { _currGame = cg })

timeStamp :: Lens' Input Double
timeStamp = lens _timeStamp (\i ts -> i { _timeStamp = ts })

key :: Lens' Input Key
key = lens _key (\i k -> i { _key = k })

isDown :: Lens' Input Bool
isDown = lens _isDown (\i d -> i { _isDown = d })

pos :: Lens' PlayerState (V2 Double)
pos = lens _pos (\ps p -> ps { _pos = p })

vel :: Lens' PlayerState (V2 Double)
vel = lens _vel (\ps v -> ps { _vel = v })

inputQueue :: Lens' PlayerState (Seq Input)
inputQueue = lens _inputQueue (\ps iq -> ps { _inputQueue = iq })

color :: Lens' PlayerState Color
color = lens _color (\ps c -> ps { _color = c })

gameName :: Lens' Game ByteString
gameName = lens _gameName (\g gn -> g { _gameName = gn })

players :: Lens' Game (Map Client PlayerState)
players = lens _players (\g ps -> g { _players = ps })

gameState :: Lens' Game GameState
gameState = lens _gameState (\g gs -> g { _gameState = gs })

clients :: Lens' ServerState (Map UUID Client)
clients = lens _clients (\s cs -> s { _clients = cs })

games :: Lens' ServerState (Map ByteString Game)
games = lens _games (\s gs -> s { _games = gs })

-- * Constants

initServerState :: ServerState
initServerState = ServerState { _clients = Map.empty, _games = Map.empty }

hello :: ByteString
hello = "\x05\x58\xE6\x39\x0A\xC4\x13\x24"
--B.pack [0x05, 0x58, 0xe6, 0x39, 0x0a, 0xc4, 0x13, 0x24]

-- * Utility functions

-- | Synonym for 'mappend'.
infixr 5 *+
(*+) :: Monoid a => a -> a -> a
(*+) = mappend
{-# INLINE (*+) #-}

-- | Replacement for the usual ternary operator. Usage:
--   @
--       let yesOrNo = condition ? "Yes" $ "No"
--   @
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
{-# INLINE (?) #-}

newClient :: UUID -> WS.Connection -> Client
newClient uid conn = Client
    { _uuid       = uid
    , _connection = conn
    , _username   = ""
    , _currGame   = ""
    }
{-# INLINE newClient #-}

newPlayerState :: PlayerState
newPlayerState = PlayerState
    { _pos        = V2 50 50
    , _vel        = zero
    , _inputQueue = Seq.empty
    , _color      = Color 0 0 0
    }

newGame :: ByteString -> Client -> Game
newGame name host = Game
    { _gameName  = name
    , _players   = Map.singleton host newPlayerState
    , _gameState = 0
    }
{-# INLINE newGame #-}

numClients :: ServerState -> Int
numClients server = Map.size $! server^.clients
{-# INLINE numClients #-}

getClient :: UUID -> ServerState -> Maybe Client
getClient uid server = (server^.clients) !? uid
{-# INLINE getClient #-}

clientExists :: Client -> ServerState -> Bool
clientExists client server = (client^.uuid) `Map.member` (server^.clients)
{-# INLINE clientExists #-}

isInGame :: Client -> Bool
isInGame client = not $! B.null (client^.currGame)
{-# INLINE isInGame #-}

addClient :: Client -> ServerState -> ServerState
addClient client server =
    server & clients %~ Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeFromGame :: Client -> ServerState -> ServerState
removeFromGame client server =
    case (server^.games) !? gameN of
        Just game ->
            if Map.size (game^.players) < 2 then
                -- We're removing the last player, so just get rid of the
                -- whole game.
                server & games %~ Map.delete gameN
            else
                -- Just remove the client from the game's player set.
                server & games %~
                    Map.adjust (& players %~ Map.delete client) gameN
        _ -> server
  where
    gameN = client^.currGame
{-# INLINE removeFromGame #-}

unregisterClient :: Client -> ServerState -> ServerState
unregisterClient client server = server & clients %~ Map.delete (client^.uuid)
{-# INLINE unregisterClient #-}

-- | Removes the given client from the given server's global register as well
--   as removes them from their currently active game, if any. If this leaves
--   the game empty, then the game is also disposed.
--
--   __Do not call this function__ unless you know you have the latest version
--   of the client in question; if you don't, call 'removeClientByUuid'
--   instead.
removeClient :: Client -> ServerState -> ServerState
removeClient client = unregisterClient client . removeFromGame client
{-# INLINE removeClient #-}

-- | Queries the server's global register for the client via UUID, and then
--   calls 'removeClient' on that fresh client. Only requires UUID, not the
--   most recent version of the client.
--
--   Does nothing (equivalent to 'id') if there is no such client with the
--   given UUID.
removeClientByUuid :: UUID -> ServerState -> ServerState
removeClientByUuid uid server =
    case (server^.clients) !? uid of
        Just client -> removeClient (client) server
        _           -> server
{-# INLINE removeClientByUuid #-}

createNewGame :: Client -> ByteString -> ServerState -> ServerState
createNewGame host name =
    let host' = host & currGame .~ name
    in  (removeFromGame host)
    >>> (addClient host')
    >>> (& games %~ Map.insert name (newGame name host'))
{-# INLINE createNewGame #-}

-- | Tricky function that updates a 'PlayerState' based on a 'Client', the
--   relevant 'Lens'', and the new value.
setPlayerState :: Client ->
                  ASetter PlayerState PlayerState a a ->
                  a ->
                  ServerState ->
                  ServerState
setPlayerState client lens' newVal =
    (& games %~
        (Map.adjust (& players %~
            (Map.adjust (& lens' .~ newVal) client))
        (client^.currGame)))
{-# INLINE setPlayerState #-}

parseByteString' :: Seq ByteString -> ByteString -> Seq ByteString
parseByteString' rs s =
    if B.null s then
        rs
    else
        let (r, t) = B.splitAt (fromIntegral (B.head s)) (B.tail s)
        in  parseByteString' (rs |> r) t
{-# INLINE parseByteString' #-}

parseByteString :: ByteString -> Seq ByteString
parseByteString = parseByteString' Seq.empty
{-# INLINE parseByteString #-}

parseColor :: ByteString -> Maybe Color
parseColor s =
    if B.length s == 3 then
        Just $! Color (B.index s 0) (B.index s 1) (B.index s 2)
    else
        Nothing
{-# INLINE parseColor #-}

-- * Primary functions

broadcast :: ByteString -> ServerState -> IO ()
broadcast msg server = sequence_ $!
    ((`WS.sendBinaryData` msg) . (^. connection)) <$> (server^.clients)

sendGameList :: ServerState -> Client -> IO ()
sendGameList server client =
    let conn = client^.connection
        getGameInfo g = B.cons (fromIntegral (B.length $! g^.gameName))
                     $! g^.gameName
                     *+ B.singleton (fromIntegral $! Map.size $! g^.players)
        gameInfoList = getGameInfo <$> Map.elems (server^.games)
    in  WS.sendBinaryData conn (B.cons 0 $! B.concat gameInfoList)

registerNewGame :: TVar ServerState -> Client -> ByteString -> IO ()
registerNewGame state host msg =
    if length parsed /= 2 || any invalidString parsed then
        WS.sendBinaryData conn ("\x01\x01" :: ByteString)
    else let [newUsername, newGameName] = (Seq.index parsed) <$> [0, 1] in
        if | B.length newUsername > 24 ->
                WS.sendBinaryData conn ("\x01\x02" :: ByteString)
           | B.length newGameName > 32 ->
                WS.sendBinaryData conn ("\x01\x03" :: ByteString)
           | otherwise -> do
                let sameUsername client = client^.username == newUsername
                server <- readTVarIO state
                if | any sameUsername $! Map.elems (server^.clients) ->
                        WS.sendBinaryData conn ("\x01\x02" :: ByteString)
                   | newGameName `Map.member` (server^.games) ->
                        WS.sendBinaryData conn ("\x01\x03" :: ByteString)
                   | otherwise -> do
                        let host' = host & username .~ newUsername
                        atomically $!
                            modifyTVar' state (createNewGame host' newGameName)

                        WS.sendBinaryData conn ("\x01\x00" :: ByteString)
  where
    conn = host^.connection
    parsed = parseByteString msg
    invalidString s = B.length s < 2 || B.any (\w -> w < 32 || w > 126) s

setGameInfo :: TVar ServerState -> Client -> ByteString -> IO ()
setGameInfo state client msg =
    case parseColor msg of
        Just c  -> atomically $!
                       modifyTVar' state (setPlayerState client color c)
        Nothing -> pure ()

commLoop :: WS.Connection -> TVar ServerState -> UUID -> IO ()
commLoop conn state uid = forever $! do
    msg <- WS.receiveData conn
    server <- readTVarIO state
    case getClient uid server of
        Just client -> unless (B.null msg) $! case B.head msg of
            0 -> isInGame client ? pure () $! sendGameList server client
            1 -> if isInGame client then
                     pure ()
                 else
                     registerNewGame state client (B.tail msg)
            2 -> setGameInfo state client (B.tail msg)
            _ -> pure ()
        _ -> throwIO $! mkIOError
                 doesNotExistErrorType
                 ("Could not find client with UUID " ++
                  show uid ++
                  " in global register")
                 Nothing
                 (Just "ZincServer.hs")

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
        let disconnect = atomically $!
                             modifyTVar' state (removeClientByUuid newUuid)

        flip finally disconnect $! do
            atomically $! modifyTVar' state $! addClient client
            commLoop conn state newUuid

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
