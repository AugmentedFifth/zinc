{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE UnboxedTuples     #-}

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
    ( -- * Constants
      hello
    , appForce
    , mass
    , friction
    , side
    , mainWidth
    , mainHeight
    , -- * Utility functions
      ($)
    , (!)
    , (*+)
    , (?)
    , ( #= )
    , (<#>)
    , ( # )
    , nullV
    , modifyTVarIO
    , newClient
    , newPlayerState
    , newGame
    , numClients
    , clientExists
    , isInGame
    , addClient
    , removeFromGame
    , createNewGame
    , -- * Data handling functions
      parseByteString
    , parseColor
    , parseInput
    , parseInputs
    , parseInputGroup
    , serializeGameState
    , -- * Primary functions
      applyInputs
    , physicsLoop
    , broadcastLoop
    , gameMainLoop
    , sendGameList
    , registerNewGame
    , setGameInfo
    , handleInputs
    , handleGameOp
    , commLoop
    , webSocketConnect
    , site
    , main
    ) where

import           Control.Applicative           (pure, (<|>))
import           Control.Arrow
import           Control.Concurrent            (ThreadId, forkIO, killThread,
                                                threadDelay)
import           Control.Concurrent.STM
import           Control.Exception             (finally, throwIO)
import           Control.Lens.Getter           ((^.))
import           Control.Lens.Lens             (Lens', lens, (&))
import           Control.Lens.Setter           (ASetter, (%~), (.~))
import           Control.Monad                 (forever, unless, void)

import qualified Data.Binary.Strict.Get        as BG
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Foldable                 (foldl', sequence_)
import           Data.Map.Strict               (Map, (!?))
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromJust, isNothing)
import           Data.Sequence                 (Seq, (|>))
import qualified Data.Sequence                 as Seq
import           Data.Serialize.IEEE754        (getFloat64le, putFloat64le)
import qualified Data.Serialize.Put            as BP
import qualified Data.Set                      as Set
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UID
import           Data.UUID.V4
import           Data.Word                     (Word32, Word8)

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V2                     (V2 (V2))
import           Linear.Vector

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Network.WebSockets.Snap

import           Prelude                       hiding (return, ($))

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe

import           System.IO.Error               (doesNotExistErrorType,
                                                mkIOError)

import           ZincData


-- * Constants

-- | The /hello/ message that all clients must send as their first packet to
--   the server.
hello :: ByteString
hello = "\x05\x58\xE6\x39\x0A\xC4\x13\x24"

-- | Fixed property of all players corresponding to the quantity of force they
--   apply with their controls.
appForce :: Double
appForce = 3e-2

-- | Fixed property of all players corresponding to their mass.
mass :: Double
mass = 12

-- | Fixed property of all players corresponding to their kinetic friction
--   along the playing surface.
friction :: Double
friction = 1e-3

-- | Fixed property of all players corresponding to their length on each side.
side :: Double
side = 48

-- | Width of playing field.
mainWidth :: Double
mainWidth = 1280

-- | Height of playing field.
mainHeight :: Double
mainHeight = 720

-- * Utility functions

-- | Replacing '$' with '$!'.
infixr 0 $
($) :: (a -> b) -> a -> b
($) = ($!)
{-# INLINE ($) #-}

-- | Infix synonym for 'Seq.index'.
infixl 9 !
(!) :: Seq a -> Int -> a
(!) = Seq.index
{-# INLINE (!) #-}

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

-- | Tricky function that is one part of updating a 'PlayerState' based on a
--   'Client', the relevant 'Lens'', and the new value.
--
--   To be used in tandem with '#' (the other part), like so:
--
--   @
--       client#color #= newColor
--   @
infixl 2 #=
( #= ) :: (# Client, ASetter PlayerState PlayerState a a #) ->
          a ->
          Game ->
          Game
( #= ) (# client, lens' #) newVal =
    (& players %~ (Map.adjust (\ps -> ps & lens' .~ newVal) client))
{-# INLINE ( #= ) #-}

-- | Like '#=', but updates instead of sets to a given value.
infixl 2 <#>
(<#>) :: (# Client, ASetter PlayerState PlayerState a a #) ->
         (a -> a) ->
         Game ->
         Game
(<#>) (# client, lens' #) updater =
    (& players %~ (Map.adjust (\ps -> ps & lens' %~ updater) client))
{-# INLINE (<#>) #-}

-- | Infix version of the unboxed 2-tuple constructor, for use with the '#='
--   and '<#>' functions.
infixl 3 #
( # ) :: a -> b -> (# a, b #)
( # ) x y = (# x, y #)
{-# INLINE ( # ) #-}

-- | Checks if a vector is null.
nullV :: (Foldable f, Epsilon a) => f a -> Bool
nullV = all nearZero
{-# INLINE nullV #-}

-- | Convenience function for modifying a 'TVar' while in the 'IO' monad.
modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO x' f = atomically $ modifyTVar' x' f
{-# INLINE modifyTVarIO #-}

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
    { _pos         = V2 50 50
    , _vel         = zero
    , _color       = Color 0 0 0
    , _inputQueue  = Seq.empty
    , _lastOrdinal = 0
    }

newGame :: ByteString -> Client -> Game
newGame name host = Game
    { _gameName        = name
    , _players         = Map.singleton host newPlayerState
    , _gameState       = 0
    , _physicsLoopId   = Nothing
    , _broadcastLoopId = Nothing
    }
{-# INLINE newGame #-}

numClients :: TVar ClientRegistry -> IO Int
numClients clientReg = readTVarIO clientReg >>= pure . Map.size
{-# INLINE numClients #-}

clientExists :: Client -> TVar ClientRegistry -> IO Bool
clientExists client clientReg =
    readTVarIO clientReg >>= pure . Map.member (client^.uuid)
{-# INLINE clientExists #-}

isInGame :: Client -> Bool
isInGame client = (not . B.null) (client^.currGame)
{-# INLINE isInGame #-}

addClient :: Client -> ClientRegistry -> ClientRegistry
addClient client = Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeFromGame :: Client -> TVar GameRegistry -> IO ()
removeFromGame client gameReg = do
    let name = client^.currGame
    games <- readTVarIO gameReg
    case games !? name of
        Just game' -> do
            game <- readTVarIO game'
            if Map.size (game^.players) < 2 then do
                -- We're removing the last player, so just get rid of the
                -- whole game.
                case game^.physicsLoopId of
                    Just threadId -> killThread threadId
                    _             -> pure ()
                case game^.broadcastLoopId of
                    Just threadId -> killThread threadId
                    _             -> pure ()
                modifyTVarIO gameReg (Map.delete name)
            else
                -- Just remove the client from the game's player set.
                modifyTVarIO game' (& players %~ Map.delete client)
        _ -> pure ()
{-# INLINE removeFromGame #-}

createNewGame :: Client ->
                 ByteString ->
                 TVar ClientRegistry ->
                 TVar GameRegistry ->
                 IO (TVar Game)
createNewGame host name clientReg gameReg = do
    let host' = host & currGame .~ name
    removeFromGame host' gameReg
    atomically $ do
        modifyTVar' clientReg (addClient host')
        game <- newTVar $ newGame name host'
        modifyTVar' gameReg (Map.insert name game)
        pure game

-- * Data handling functions

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
        Just $ Color (B.index s 0) (B.index s 1) (B.index s 2)
    else
        Nothing
{-# INLINE parseColor #-}

parseInput :: ByteString -> (# Maybe Input, ByteString #)
parseInput s =
    case BG.runGet getInput s of
        (Right (# timeStamp', key', isDown' #), t) ->
            (# Just $ Input
                   { _timeStamp = timeStamp'
                   , _key       = getKey key'
                   , _isDown    = isDown' /= 0
                   }
            ,  t
            #)
        (_, t) -> (# Nothing, t #)
  where
    getInput = do
        timeStamp' <- getFloat64le
        key'       <- BG.getWord8
        isDown'    <- BG.getWord8
        pure (# timeStamp', key', isDown' #)

    getKey k =
        case k of
            0 -> V2 0    (-1)
            1 -> V2 (-1) 0
            2 -> V2 0    1
            3 -> V2 1    0

parseInputs :: ByteString -> Seq Input
parseInputs s =
    case parseInput s of
        (# Just input, t #) -> input <| parseInputs t
        _                   -> Seq.empty

parseInputGroup :: ByteString -> Maybe InputGroup
parseInputGroup s =
    case maybeHeader of
        Just (# ordinal', now', dt', t #) ->
            Just $ InputGroup
                { _ordinal = ordinal'
                , _now     = now'
                , _dt      = dt'
                , _inputs  = parseInputs t
                }
        _ -> Nothing
  where
    maybeHeader = case BG.runGet getInputHeader s of
        (Right (# ordinal', now', dt' #), t) ->
            Just (# ordinal', now', dt', t #)
        _ ->
            Nothing
    getInputHeader = do
        ordinal' <- BG.getWord32le
        now'     <- getFloat64le
        dt'      <- getFloat64le
        pure (# ordinal', now', dt' #)

serializeGameState :: Game -> ByteString
serializeGameState game =
    B.cons 2 $ Map.foldlWithKey' foldSerializePlayerState "" (game^.players)
  where
    foldSerializePlayerState accu client ps =
        let V2 px py = ps^.pos
            V2 vx vy = ps^.vel
            Color r g b = ps^.color
        in  mappend accu $ BP.runPut $
            do
                BP.putByteString $ UID.toByteString (client^.uuid)
                putFloat64le px
                putFloat64le py
                putFloat64le vx
                putFloat64le vy
                BP.putWord8 r
                BP.putWord8 g
                BP.putWord8 b
                BP.putWord32le $ ps^.lastOrdinal

-- * Primary functions

-- | what
applyInputs :: PlayerState -> PlayerState
applyInputs ps =
    foldl' (\ps' inputGroup ->
        -- Calculate accelerations for this frame based on input log.
        let (# v, pressed, _, t_ #) = foldl' (\(# v, pressed, t0', t_ #) inp ->
                let t1 = inp^.timeStamp
                    down = inp^.isDown
                in  (# case t0' of
                           Just t0 -> let thisDt = t1 - t0 in
                               if thisDt > 0 then
                                   let dir = (normalize . sum) pressed
                                   in  v + dir ^* (appForce / mass * thisDt)
                               else
                                   v
                           _ -> v
                    ,  (down ? Set.insert $ Set.delete) (inp^.key) pressed
                    ,  Just t1
                    ,  if down then case t0' of
                           Just _ -> Just t_
                           _      -> Just t1
                       else
                           Just t_
                    #)
                ) (# ps'^.vel, Set.empty, Nothing, Nothing #)
                  (inputGroup^.inputs)
            leftoverDir = (normalize . sum) pressed
            v' = if nullV leftoverDir then
                     v
                 else let thisDt = (inputGroup^.now) - t_ in
                     v + leftoverDir ^* (appForce / mass * thisDt)
            -- Apply frictional forces.
            dt' = inputGroup^.dt
            v'' = let frictionalDvNorm = -friction * dt'
                  in if nullV v' || abs frictionalDvNorm >= norm v' then
                      zero
                  else
                      v' + normalize v' ^* frictionalDvNorm
            -- Update position based on new velocity.
            pos' = (ps'^.pos) + v'' ^* dt'
            -- Collision detection.
            (# v''', pos'' #) =
                let V2 vx vy = v''
                    V2 px py = pos'
                    (# vy', py' #) = if
                        | py <= 0                 -> -vy # 0
                        | py >= mainHeight - side -> -vy # mainHeight - side
                        | otherwise               ->  vy # py
                    (# vx', px' #) = if
                        | px >= mainWidth  - side -> -vx # mainWidth  - side
                        | px <= 0                 -> -vx # 0
                        | otherwise               ->  vx # px
                in  V2 vx' vy' # V2 px' py'
        in  ps' & pos         .~ pos''
                & vel         .~ v'''
                & lastOrdinal %~ (max (inputGroup^.ordinal))
    ) ps (ps^.inputQueue) & inputQueue .~ Seq.empty

physicsLoop :: TVar Game -> IO ()
physicsLoop game' = forever $ do
    threadDelay 15000
    atomically $ modifyTVar' game' (& players %~ Map.map applyInputs)
{-# INLINE physicsLoop #-}

broadcastLoop :: ByteString -> TVar Game -> IO ()
broadcastLoop name game' = forever $ do
    threadDelay 45000
    game <- readTVarIO game'
    let s = serializeGameState game
    let sendPacket client _ = WS.sendBinaryData (client^.connection) s
    Map.traverseWithKey sendPacket (game^.players)
{-# INLINE broadcastLoop #-}

gameMainLoop :: TVar Game -> IO ()
gameMainLoop game' = do
    physicsLoopId'   <- forkIO $ physicsLoop   game'
    broadcastLoopId' <- forkIO $ broadcastLoop game'
    atomically $ modifyTVar' game' (& physicsLoopId   .~ Just physicsLoopId'
                                    & broadcastLoopId .~ Just broadcastLoopId')
{-# INLINE gameMainLoop #-}

sendGameList :: TVar GameRegistry -> Client -> IO ()
sendGameList gameReg client = do
    games <- readTVarIO gameReg
    gameInfoList <- atomically (traverse getGameInfo (Map.elems games))
    WS.sendBinaryData conn (B.cons 0 $ B.concat gameInfoList)
  where
    conn = client^.connection
    getGameInfo g' = do
        g <- readTVar g'
        pure $ B.cons (fromIntegral (B.length $ g^.gameName))
             $ g^.gameName
            *+ B.singleton (fromIntegral $ Map.size $ g^.players)

registerNewGame :: TVar ClientRegistry ->
                   TVar GameRegistry ->
                   Client ->
                   ByteString ->
                   IO ()
registerNewGame clientReg gameReg host msg =
    if length parsed /= 2 || any invalidString parsed then
        WS.sendBinaryData conn one
    else let (# newUsername, newGameName #) = parsed ! 0 # parsed ! 1 in if
        | B.length newUsername > 24 -> WS.sendBinaryData conn two
        | B.length newGameName > 32 -> WS.sendBinaryData conn three
        | otherwise -> do
            let sameUsername client = client^.username == newUsername
            clients <- readTVarIO clientReg
            games   <- readTVarIO gameReg
            if | any sameUsername clients     -> WS.sendBinaryData conn two
               | Map.member newGameName games -> WS.sendBinaryData conn three
               | otherwise -> do
                    let host' = host & username .~ newUsername
                    game' <- createNewGame host' newGameName clientReg gameReg
                    gameMainLoop game'
                    WS.sendBinaryData conn zero'
  where
    conn = host^.connection
    parsed = parseByteString msg
    invalidString s = B.length s < 2 || B.any (\w -> w < 32 || w > 126) s
    zero' = "\x01\x00" :: ByteString
    one   = "\x01\x01" :: ByteString
    two   = "\x01\x02" :: ByteString
    three = "\x01\x03" :: ByteString

setGameInfo :: Client -> ByteString -> TVar Game -> IO ()
setGameInfo client msg game' =
    case parseColor msg of
        Just c  -> modifyTVarIO game' (client#color #= c)
        Nothing -> pure ()
{-# INLINE setGameInfo #-}

handleInputs :: Client -> ByteString -> TVar Game -> IO ()
handleInputs client msg game' =
    case parseInputGroup msg of
        Just inputG -> modifyTVarIO game' (client#inputQueue <#> (|> inputG))
        Nothing     -> pure ()
{-# INLINE handleInputs #-}

handleGameOp :: Word8 -> ByteString -> Client -> TVar GameRegistry -> IO ()
handleGameOp opcode msg client gameReg = do
    games <- readTVarIO gameReg
    case games !? (client^.currGame) of
        Just game' -> case opcode of
            2 -> setGameInfo client (B.tail msg) game'
            3 -> handleInputs client (B.tail msg) game'
            _ -> pure ()
        _ -> pure ()

commLoop :: WS.Connection ->
            TVar ClientRegistry ->
            TVar GameRegistry ->
            UUID ->
            IO ()
commLoop conn clientReg gameReg uid = forever $ do
    msg <- WS.receiveData conn
    clients <- readTVarIO clientReg
    case clients !? uid of
        Just client -> unless (B.null msg) $ case B.head msg of
            0 -> isInGame client ? pure () $ sendGameList gameReg client
            1 -> if isInGame client then
                     pure ()
                 else
                     registerNewGame clientReg gameReg client (B.tail msg)
            4 -> removeFromGame client gameReg
            n -> forkIO $ handleGameOp n msg client gameReg
        _ -> throwIO $ mkIOError
                 doesNotExistErrorType
                 ("Could not find client with UUID " ++
                  show uid ++
                  " in global register")
                 Nothing
                 (Just "ZincServer.hs")

webSocketConnect :: TVar ClientRegistry -> TVar GameRegistry -> WS.ServerApp
webSocketConnect clientReg gameReg pending = do
    conn <- WS.acceptRequest pending
    msg  <- WS.receiveData conn

    if msg /= hello then
        WS.sendCloseCode conn 1002 ("Wrong announcement" :: ByteString)
    else do
        WS.forkPingThread conn 30
        newUuid <- nextRandom
        WS.sendBinaryData conn $ UID.toByteString newUuid
        print newUuid

        let client = newClient newUuid conn
        let disconnect = atomically $ do
                clients <- readTVar clientReg
                case clients !? newUuid of
                    Just client' -> removeFromGame client' gameReg
                    _            -> pure ()
                modifyTVar' clientReg (Map.delete newUuid)

        flip finally disconnect $ do
            atomically $ modifyTVar' clientReg $ addClient client
            commLoop conn clientReg gameReg newUuid

site :: TVar ClientRegistry -> TVar GameRegistry -> Snap ()
site clients games =
        ifTop (serveFile "public/index.html")
    <|> route [ ("ws", runWebSocketsSnap $ webSocketConnect clients games)
              , ("",   serveDirectory "public")
              ]

main :: IO ()
main = do
    clientReg <- atomically $ newTVar Map.empty
    gameReg <- atomically $ newTVar Map.empty
    httpServe (setPort 3000 mempty) (site clientReg gameReg)
