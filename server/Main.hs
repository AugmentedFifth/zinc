{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Main where

import           Control.Applicative           (pure, (<|>))
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Lens.Getter           ((^.))
import           Control.Lens.Lens             (Lens', lens, (&))
import           Control.Lens.Setter           ((%~))
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
    }
{-# INLINE newClient #-}

numClients :: ServerState -> Int
numClients server = Map.size $ server^.clients
{-# INLINE numClients #-}

clientExists :: Client -> ServerState -> Bool
clientExists client server = (client^.uuid) `Map.member` (server^.clients)
{-# INLINE clientExists #-}

addClient :: Client -> ServerState -> ServerState
addClient client server =
    server & clients %~ Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeClient :: Client -> ServerState -> ServerState
removeClient client server = server & clients %~ Map.delete (client^.uuid)
{-# INLINE removeClient #-}

{- Primary functions -}
broadcast :: ByteString -> ServerState -> IO ()
broadcast msg server = sequence_ $
    ((`WS.sendBinaryData` msg) . (^. connection)) <$> (server^.clients)

sendGameList :: ServerState -> Client -> IO ()
sendGameList state client =
    let conn = client^.connection
        getGameInfo g = B.cons (fromIntegral (B.length $ g^.gameName))
                      $ g^.gameName
                     *+ B.singleton (fromIntegral $ Set.size $ g^.players)
        gameInfoList = getGameInfo <$> Map.elems (state^.games)
    in  WS.sendBinaryData conn (B.cons 0 $ B.concat gameInfoList)

commLoop :: WS.Connection -> TVar ServerState -> Client -> IO ()
commLoop conn tState client = forever $ do
    msg <- WS.receiveData conn
    state <- readTVarIO tState
    unless (B.null msg) $ case B.head msg of
        0 -> sendGameList state client
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
        WS.sendBinaryData conn $ UID.toByteString newUuid
        print newUuid

        let client = newClient newUuid conn
        let disconnect = atomically $ modifyTVar' state (removeClient client)

        flip finally disconnect $ do
            atomically $ modifyTVar' state $ addClient client
            commLoop conn state client

site :: TVar ServerState -> Snap ()
site state =
        ifTop (serveFile "public/index.html")
    <|> route [ ("ws", runWebSocketsSnap $ webSocketConnect state)
              , ("",   serveDirectory "public")
              ]

main :: IO ()
main = do
    state <- atomically $ newTVar initServerState
    httpServe (setPort 3000 mempty) (site state)
