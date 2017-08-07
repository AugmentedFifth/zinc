{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE UnboxedSums       #-}
{-# LANGUAGE UnboxedTuples     #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-| Module      : ZincServer
    Description : Server for the /zinc/ game.
    Copyright   : [copyleft] AugmentedFifth, 2017
    License     : AGPL-3
    Maintainer  : zcomito@gmail.com
    Stability   : experimental
    Portability : POSIX
-}
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
    , -- * Data handling functions
      parseByteString
    , parseColor
    , parseInput
    , parseInputs
    , parseClick
    , parseClicks
    , parseInputGroup
    , serializeGameState
    , -- * Primary functions
      isInGame
    , addClient
    , removeFromGame
    , createNewGame
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
import           Control.Concurrent            (forkIO, killThread,
                                                threadDelay)
import           Control.Concurrent.STM
import           Control.Exception             (finally, throwIO)
import           Control.Lens.Getter           ((^.))
import           Control.Lens.Setter           (ASetter, (%~), (.~))
import           Control.Monad                 (forever, unless, void)

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Foldable                 (foldl', for_)
import           Data.Function                 ((&))
import           Data.Map.Strict               (Map, (!?), foldlWithKey')
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromJust)
import           Data.Sequence                 (Seq, Seq(..), (<|), (|>))
import qualified Data.Sequence                 as Seq
import qualified Data.Serialize.Get            as BG
import           Data.Serialize.IEEE754        (getFloat64le, putFloat64le)
import qualified Data.Serialize.Put            as BP
import qualified Data.Set                      as Set
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UID
import           Data.UUID.V4
import           Data.Word                     (Word8, Word32)

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V2                     (V2 (V2))
import           Linear.Vector

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Network.WebSockets.Snap

import           Prelude                       hiding (return, ($))

import           Snap.Core                     hiding (dir)
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

-- | Maximum time a player can charge a shot.
maxHoldTime :: Double
maxHoldTime = 2000

-- | Maximum velocity of a fired projectile.
maxProjVel :: Double
maxProjVel = 2.5

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
    (& players %~ Map.adjust (\ps -> ps & lens' .~ newVal) client)
{-# INLINE ( #= ) #-}

-- | Like '#=', but updates instead of sets to a given value.
infixl 2 <#>
(<#>) :: (# Client, ASetter PlayerState PlayerState a a #) ->
         (a -> a) ->
         Game ->
         Game
(<#>) (# client, lens' #) updater =
    (& players %~ Map.adjust (\ps -> ps & lens' %~ updater) client)
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

invalidString :: Int -> ByteString -> Bool
invalidString minLen s =
    B.length s < minLen || B.any (\w -> w < 32 || w > 126) s
{-# INLINE invalidString #-}

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
    , _projectiles = Seq.empty
    , _lastOrdinal = 0
    }
{-# INLINE newPlayerState #-}

newGame :: ByteString -> Client -> Game
newGame name host = Game
    { _gameName        = name
    , _players         = Map.singleton host newPlayerState
    , _inputQueue      = Seq.empty
    , _gameState       = 0
    , _physicsLoopId   = Nothing
    , _broadcastLoopId = Nothing
    }
{-# INLINE newGame #-}

-- * Data handling functions

parseByteString' :: Seq ByteString -> ByteString -> Seq ByteString
parseByteString' rs s =
    if B.null s then
        rs
    else
        let
            (r, t) = B.splitAt (fromIntegral (B.head s)) (B.tail s)
        in
            parseByteString' (rs |> r) t
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
    case BG.runGetState getInput s 0 of
        Right ((timeStamp', key', isDown'), t) ->
            (# Just Input
                   { _timeStamp = timeStamp'
                   , _key       = getKey key'
                   , _isDown    = isDown' /= 0
                   }
            ,  t
            #)

        _ ->
            Nothing # s
  where
    getInput = do
        timeStamp' <- getFloat64le
        key'       <- BG.getWord8
        isDown'    <- BG.getWord8
        pure (timeStamp', key', isDown')

    getKey k =
        case k of
            0 -> V2 0    (-1)
            1 -> V2 (-1) 0
            2 -> V2 0    1
            3 -> V2 1    0
            _ -> V2 0    0
{-# INLINE parseInput #-}

parseInputs :: Word8 -> ByteString -> (# Seq Input, ByteString #)
parseInputs count s =
    if count > 0 then
        case parseInput s of
            (# Just input, t #) ->
                let
                    (# inputs', t' #) = parseInputs (count - 1) t
                in
                    input <| inputs' # t'

            (# _, t #) ->
                Seq.empty # t
    else
        Seq.empty # s
{-# INLINE parseInputs #-}

parseClick :: ByteString -> (# Maybe Click, ByteString #)
parseClick s =
    case BG.runGetState getClick s 0 of
        Right (click, t) -> Just click # t
        _                -> Nothing    # s
  where
    getClick = do
        isDown'   <- BG.getWord8
        timestamp <- getFloat64le
        if isDown' == 0 then do
            clickId <- BG.getWord32le
            pure $ Click timestamp (# clickId | #)
        else do
            clickX <- getFloat64le
            clickY <- getFloat64le
            pure $ Click timestamp (# | V2 clickX clickY #)
{-# INLINE parseClick #-}

parseClicks :: ByteString -> Seq Click
parseClicks s =
    case parseClick s of
        (# Just click, t #) -> click <| parseClicks t
        _                   -> Seq.empty
{-# INLINE parseClicks #-}

parseInputGroup :: Client -> ByteString -> Maybe InputGroup
parseInputGroup client s =
    case maybeHeader of
        Just (ordinal', now', dt', keypressCount, t) ->
            let (# inputs', t' #) = parseInputs keypressCount t in
                Just InputGroup
                    { _client' = client
                    , _ordinal = ordinal'
                    , _now     = now'
                    , _dt      = dt'
                    , _inputs  = inputs'
                    , _clicks  = parseClicks t'
                    }
        _ -> Nothing
  where
    maybeHeader = case BG.runGetState getInputHeader s 0 of
        Right ((ordinal', now', dt', keypressCount), t) ->
            Just (ordinal', now', dt', keypressCount, t)
        _ ->
            Nothing
    getInputHeader = do
        ordinal'      <- BG.getWord32le
        now'          <- getFloat64le
        dt'           <- getFloat64le
        keypressCount <- BG.getWord8
        pure (ordinal', now', dt', keypressCount)
{-# INLINE parseInputGroup #-}

serializeProjectiles :: Seq Projectile -> ByteString
serializeProjectiles pjs =
    BP.runPut $ for_ pjs $ \(Projectile id' (V2 px py) (V2 vx vy) phase) -> do
        BP.putWord32le id'
        putFloat64le px
        putFloat64le py
        putFloat64le vx
        putFloat64le vy
        BP.putWord8 phase
{-# INLINE serializeProjectiles #-}

serializeGameState :: Game -> ByteString
serializeGameState game =
    B.cons 2 $ foldlWithKey' foldSerializePlayerState "" (game^.players)
  where
    foldSerializePlayerState accu client ps =
        let
            V2 px py = ps^.pos
            V2 vx vy = ps^.vel
            Color r g b = ps^.color
            username' = client^.username
            projectiles' = ps^.projectiles
            serialProjectiles = serializeProjectiles projectiles'
        in
            mappend accu $ BP.runPut $ do
                BP.putWord8 $ fromIntegral $ B.length username'
                BP.putByteString username'
                putFloat64le px
                putFloat64le py
                putFloat64le vx
                putFloat64le vy
                BP.putWord8 r
                BP.putWord8 g
                BP.putWord8 b
                BP.putWord8 $ fromIntegral $ Seq.length projectiles'
                BP.putByteString serialProjectiles
                BP.putWord32le $ ps^.lastOrdinal
{-# INLINE serializeGameState #-}

-- * Primary functions

isInGame :: Client -> Bool
isInGame client = (not . B.null) (client^.currGame)
{-# INLINE isInGame #-}

addClient :: Client -> ClientRegistry -> ClientRegistry
addClient client = Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeFromGame :: Client -> TVar GameRegistry -> IO Client
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
    pure $ client & currGame .~ ""
{-# INLINE removeFromGame #-}

createNewGame :: Client ->
                 ByteString ->
                 TVar ClientRegistry ->
                 TVar GameRegistry ->
                 IO (TVar Game)
createNewGame host name clientReg gameReg = do
    host' <- removeFromGame host gameReg
    let host'' = host' & currGame .~ name
    atomically $ do
        modifyTVar' clientReg (addClient host'')
        game <- newTVar $ newGame name host''
        modifyTVar' gameReg (Map.insert name game)
        pure game
{-# INLINE createNewGame #-}

processJoinGame :: Client ->
                   ByteString ->
                   TVar ClientRegistry ->
                   TVar GameRegistry ->
                   IO ()
processJoinGame joiner name clientReg gameReg = do
    joiner' <- removeFromGame joiner gameReg
    let joiner'' = joiner' & currGame .~ name
    atomically $ do
        modifyTVar' clientReg (addClient joiner'')
        games <- readTVar gameReg
        case games !? name of
            Just game' ->
                modifyTVar' game'
                    (& players %~ Map.insert joiner'' newPlayerState)
            _ ->
                pure ()
{-# INLINE processJoinGame #-}

updateProjectile :: Map Client PlayerState ->
                    Double ->
                    Projectile ->
                    (# Projectile, Map Client PlayerState #)
updateProjectile players' dt' (Projectile id' p v@(V2 vx vy) 0) =
    let
        V2 px py = p + v ^* dt'

        -- Collision detection with arena bounds.
        (# vy', py', hitY #) = if
            | py <= 0          -> (# -vy, 0,          True  #) -- top
            | py >= mainHeight -> (# -vy, mainHeight, True  #) -- bottom
            | otherwise        -> (#  vy, py,         False #) -- none
        (# vx', px', hitX #) = if
            | px <= 0          -> (# -vx, 0,          True  #) -- left
            | px >= mainWidth  -> (# -vx, mainWidth,  True  #) -- right
            | otherwise        -> (#  vx, px,         False #) -- none

        -- Collision detection with players.
        (v', p', hitPlayer, players'') =
            foldlWithKey' (\(v1@(V2 vx'' vy''),
                             p1@(V2 px'' py''),
                             hitPlayer',
                             players''') c ps ->
                let op1@(V2 opx opy) = ps^.pos in
                    if
                        py'' < opy        ||
                        py'' > opy + side ||
                        px'' < opx        ||
                        px'' > opx + side
                    then
                        (v1, p1, hitPlayer', Map.insert c ps players''')
                    else
                        let
                            obstacleCenter = op1 + pure (side / 2)

                            V2 dispX dispY = p1 - obstacleCenter

                            dispVel = v1 ^/ maxProjVel

                            ps' = ps & vel %~ (+ dispVel)

                            players'''' = Map.insert c ps' players'''
                        in
                            if abs dispY > abs dispX then
                                if py'' < opy + side / 2 then -- top
                                    ( V2 vx'' (-vy'')
                                    , V2 px'' opy
                                    , True
                                    , players''''
                                    )
                                else                          -- bottom
                                    ( V2 vx'' (-vy'')
                                    , V2 px'' (opy + side)
                                    , True
                                    , players''''
                                    )
                            else
                                if px'' < opx + side / 2 then -- left
                                    ( V2 (-vx'') vy''
                                    , V2 opx py''
                                    , True
                                    , players''''
                                    )
                                else                          -- right
                                    ( V2 (-vx'') vy''
                                    , V2 (opx + side) py''
                                    , True
                                    , players''''
                                    )
            ) (V2 vx' vy', V2 px' py', False, Map.empty) players'

    in
        Projectile id' p' v' (hitY || hitX || hitPlayer ? 1 $ 0) # players''

updateProjectile players' _ pj = pj # players'
{-# INLINE updateProjectile #-}

notDestroyed :: Projectile -> Bool
notDestroyed (Projectile _ _ _ phase) = phase < 2
{-# INLINE notDestroyed #-}

updateProjectiles :: Map Client PlayerState ->
                     Double ->
                     Seq Projectile ->
                     (Seq Projectile, Map Client PlayerState)
updateProjectiles players' dt' projectiles' =
    foldl' (\(pjs, players'') pj ->
        let
            (# pj', players''' #) = updateProjectile players'' dt' pj
        in
            (pjs |> pj', players''')
    ) (Seq.empty, players') (Seq.filter notDestroyed projectiles')
{-# INLINE updateProjectiles #-}

checkPlayerCollision :: Client ->
                        Map Client PlayerState ->
                        V2 Double ->
                        V2 Double ->
                        (V2 Double, V2 Double, Map Client PlayerState)
checkPlayerCollision client players' p v =
    foldlWithKey' (\(p'@(V2 px py), v', players'') c ps ->
        let op'@(V2 opx opy) = ps^.pos in
            if
                py < opy - side ||
                py > opy + side ||
                px < opx - side ||
                px > opx + side ||
                c == client
            then
                (p', v', players'')
            else
                let
                    V2 dispX dispY = p' - op'

                    p'' =
                        if abs dispY > abs dispX then
                            if py < opy - side / 2 then
                                V2 px (opy - side)
                            else
                                V2 px (opy + side)
                        else
                            if px < opx - side / 2 then
                                V2 (opx - side) py
                            else
                                V2 (opx + side) py

                    ov = ps^.vel

                    disp1 = p'' - op'
                    disp2 = op' - p''

                    projection1 = (v' - ov) `dot` disp1 / quadrance disp1
                    projection2 = (ov - v') `dot` disp2 / quadrance disp2

                    massRatio = 1 -- 2 * mass1 / (mass1 + mass2)

                    v1 = v' - projection1 * massRatio *^ disp1
                    v2 = ov - projection2 * massRatio *^ disp2
                in
                    (p'', v1, Map.adjust (& vel .~ v2) c players'')
    ) (p, v, players') players'
{-# INLINE checkPlayerCollision #-}

checkBoundsCollision :: V2 Double -> V2 Double -> (# V2 Double, V2 Double #)
checkBoundsCollision (V2 px py) (V2 vx vy) = V2 px' py' # V2 vx' vy'
  where
    (# vy', py' #) = if
        | py <= 0                 -> -vy # 0
        | py >= mainHeight - side -> -vy # mainHeight - side
        | otherwise               ->  vy # py
    (# vx', px' #) = if
        | px >= mainWidth  - side -> -vx # mainWidth  - side
        | px <= 0                 -> -vx # 0
        | otherwise               ->  vx # px
{-# INLINE checkBoundsCollision #-}

updatePos :: Double -> V2 Double -> V2 Double -> V2 Double
updatePos dt' v pos' = pos' + v ^* dt'
{-# INLINE updatePos #-}

applyFriction :: Double -> V2 Double -> V2 Double
applyFriction dt' v =
    if nullV v || abs frictionalDvNorm >= norm v then
        zero
    else
        v + normalize v ^* frictionalDvNorm
  where
    frictionalDvNorm = -friction * dt'
{-# INLINE applyFriction #-}

applyAcceleration :: V2 Double -> Seq Input -> Double -> V2 Double
applyAcceleration v' inputs' now' =
    let (v'', pressed', _, t_') = foldl' (\(v, pressed, t0', t_) inp ->
            let
                t1   = inp^.timeStamp

                down = inp^.isDown
            in
                ( case t0' of
                      Just t0 -> let thisDt = t1 - t0 in
                          if thisDt > 0 then
                              let
                                  dir = (normalize . sum) pressed
                              in
                                  v + dir ^* (appForce / mass * thisDt)
                          else
                              v
                      _ -> v
                , (if down then Set.insert else Set.delete) (inp^.key) pressed
                , Just t1
                , if down then case t0' of
                      Just _ -> t_
                      _      -> Just t1
                  else
                      t_
                )
            ) (v', Set.empty, Nothing, Nothing) inputs'

        leftoverDir = (normalize . sum) pressed'
    in
        if nullV leftoverDir then
            v''
        else
            let
                thisDt = now' - fromJust t_'
            in
                v'' + leftoverDir ^* (appForce / mass * thisDt)
{-# INLINE applyAcceleration #-}

spawnProjectiles' :: Maybe Double ->
                     Maybe Word32 ->
                     Seq Projectile ->
                     V2 Double ->
                     V2 Double ->
                     Seq Click ->
                     (# Maybe Double
                     ,  Maybe Word32
                     ,  Seq Projectile
                     ,  V2 Double
                     #)
spawnProjectiles' lastPress' cid' pjs _ v Empty =
    (# lastPress', cid', pjs, v #)

spawnProjectiles' lastPress'
                  cid'
                  pjs
                  pos'
                  v
                  (Click timestamp idOrPos :<| clicks') =
    let
        (# lastPress'', cid, maybeClickPos #) =
            case idOrPos of
                (# cid'' |   #) -> (# Just timestamp, Just cid'', Nothing #)
                (#       | p #) -> (# Nothing,        Nothing,    Just p  #)

        maybePjAndNewVel = do
            clickPos  <- maybeClickPos
            lastPress <- lastPress'
            cid'''    <- cid'
            let mouseDt = timestamp - lastPress
            let playerCenter = pos' + pure (side / 2)
            let dir = normalize $ clickPos - playerCenter
            if nullV dir then
                Nothing
            else
                let
                    ratio = min mouseDt maxHoldTime / maxHoldTime
                    startPos = playerCenter + side *^ dir
                    projVel = maxProjVel * ratio *^ dir
                    v1 = v + dir ^* (-ratio)
                in
                    pure (Projectile cid''' startPos projVel 0, v1)

        (# pjs', newVel #) =
            case maybePjAndNewVel of
                Just (pj', newVel') -> pjs |> pj' # newVel'
                _                   -> pjs        # v
    in
        spawnProjectiles' lastPress'' cid pjs' pos' newVel clicks'
{-# INLINE spawnProjectiles' #-}

spawnProjectiles :: Seq Projectile ->
                    V2 Double ->
                    V2 Double ->
                    Seq Click ->
                    (# Seq Projectile, V2 Double #)
spawnProjectiles pjs p v clicks' = pjs' # v'
  where
    (# _, _, pjs', v' #) = spawnProjectiles' Nothing Nothing pjs p v clicks'
{-# INLINE spawnProjectiles #-}

updateGameState :: Game -> Game
updateGameState game =
    foldl' (\game' inputGroup ->
        let
            client = inputGroup^.client'

            playerState' = (game'^.players) !? client
        in
            case playerState' of
                Just playerState ->
                    let
                        origPos = playerState^.pos

                        (# newProjs, knockBackVel #) =
                            spawnProjectiles
                                (playerState^.projectiles)
                                origPos
                                (playerState^.vel)
                                (inputGroup^.clicks)

                        dt' = inputGroup^.dt

                        acceledVel =
                            applyAcceleration
                                knockBackVel
                                (inputGroup^.inputs)
                                (inputGroup^.now)

                        frictionedVel = applyFriction dt' acceledVel

                        newPos = updatePos dt' frictionedVel origPos

                        (# boundedPos, boundedVel #) =
                            checkBoundsCollision newPos frictionedVel

                        (crashedPos, crashedVel, newPlayers) =
                            checkPlayerCollision
                                client
                                (game'^.players)
                                boundedPos
                                boundedVel

                        newPlayers' =
                            Map.adjust
                                (\pl -> pl & pos .~ crashedPos
                                           & vel .~ crashedVel)
                                client
                                newPlayers

                        (updatedProjs, newPlayers'') =
                            updateProjectiles newPlayers' dt' newProjs

                        newPlayers''' =
                            Map.adjust
                                (\pl -> pl & projectiles .~ updatedProjs
                                           & lastOrdinal %~
                                                 max (inputGroup^.ordinal))
                                client
                                newPlayers''
                    in
                        game' & players .~ newPlayers'''

                _ ->
                    game'
    ) game (game^.inputQueue) & inputQueue .~ Seq.empty

physicsLoop :: TVar Game -> IO ()
physicsLoop game' = forever $ do
    threadDelay 15000
    modifyTVarIO game' updateGameState
{-# INLINE physicsLoop #-}

setProjsBroadcast :: Game -> Game
setProjsBroadcast =
    (& players %~ fmap
        (& projectiles %~ fmap (\case
            Projectile id' p v 1 -> Projectile id' p v 2
            pj                   -> pj)))
{-# INLINE setProjsBroadcast #-}

broadcastLoop :: TVar Game -> IO ()
broadcastLoop game' = forever $ do
    threadDelay 45000
    game <- atomically $ do
        modifyTVar' game' setProjsBroadcast
        readTVar game'
    let s = serializeGameState game
    let sendPacket client _ = WS.sendBinaryData (client^.connection) s
    Map.traverseWithKey sendPacket (game^.players)
{-# INLINE broadcastLoop #-}

gameMainLoop :: TVar Game -> IO ()
gameMainLoop game' = do
    physicsLoopId'   <- forkIO $ physicsLoop   game'
    broadcastLoopId' <- forkIO $ broadcastLoop game'
    atomically $ modifyTVar' game' $ \game ->
        game & physicsLoopId   .~ Just physicsLoopId'
             & broadcastLoopId .~ Just broadcastLoopId'
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
    if length parsed /= 2 || any (invalidString 2) parsed then
        WS.sendBinaryData conn one
    else let (# newUsername, newGameName #) = parsed ! 0 # parsed ! 1 in if
        | B.length newUsername > 24 -> WS.sendBinaryData conn two
        | B.length newGameName > 32 -> WS.sendBinaryData conn three
        | otherwise -> do
            let sameUsername client = client^.username == newUsername
            clients <- readTVarIO clientReg
            games   <- readTVarIO gameReg
            if | host^.username /= newUsername && any sameUsername clients ->
                    WS.sendBinaryData conn two
               | Map.member newGameName games -> WS.sendBinaryData conn three
               | otherwise -> do
                    let host' = host & username .~ newUsername
                    game' <- createNewGame host' newGameName clientReg gameReg
                    gameMainLoop game'
                    WS.sendBinaryData conn zero'
  where
    conn = host^.connection
    parsed = parseByteString msg
    zero' = "\x01\x00" :: ByteString
    one   = "\x01\x01" :: ByteString
    two   = "\x01\x02" :: ByteString
    three = "\x01\x03" :: ByteString

joinGame :: TVar ClientRegistry ->
            TVar GameRegistry ->
            Client ->
            ByteString ->
            IO ()
joinGame clientReg gameReg joiner msg =
    if length parsed /= 2 || any (invalidString 2) parsed then
        WS.sendBinaryData conn one
    else let (# newUsername, toJoin #) = parsed ! 0 # parsed ! 1 in
        if B.length newUsername > 24 then
            WS.sendBinaryData conn two
        else do
            let sameUsername client = client^.username == newUsername
            clients <- readTVarIO clientReg
            games   <- readTVarIO gameReg
            if | joiner^.username /= newUsername && any sameUsername clients ->
                    WS.sendBinaryData conn two
               | toJoin `Map.notMember` games -> WS.sendBinaryData conn three
               | otherwise -> do
                    let joiner' = joiner & username .~ newUsername
                    processJoinGame joiner' toJoin clientReg gameReg
                    WS.sendBinaryData conn zero'
  where
    conn = joiner^.connection
    parsed = parseByteString msg
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
    case parseInputGroup client msg of
        Just inputG -> modifyTVarIO game' (& inputQueue %~ (|> inputG))
        Nothing     -> pure ()
{-# INLINE handleInputs #-}

sendChat :: Client -> ByteString -> TVar Game -> IO ()
sendChat client msg game' =
    if B.length msg > 96 || invalidString 1 msg then
        pure ()
    else do
        let clientName = client^.username
        let nameLength = (fromIntegral . B.length) clientName
        let msg' = B.cons 3 (B.cons nameLength clientName) *+ msg
        let sendPacket client'' _ =
                WS.sendBinaryData (client''^.connection) msg'
        game <- readTVarIO game'
        void $ Map.traverseWithKey sendPacket (game^.players)

handleGameOp :: Word8 -> ByteString -> Client -> TVar GameRegistry -> IO ()
handleGameOp opcode msg client gameReg = do
    games <- readTVarIO gameReg
    case games !? (client^.currGame) of
        Just game' -> case opcode of
            2 -> setGameInfo  client messageBody game'
            3 -> handleInputs client messageBody game'
            6 -> sendChat     client messageBody game'
            _ -> pure ()
        _ -> pure ()
  where
    messageBody = B.tail msg

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
            4 -> do  client'' <- removeFromGame client gameReg
                     modifyTVarIO clientReg (Map.insert uid client'')
            5 -> if isInGame client then
                     pure ()
                 else
                     joinGame clientReg gameReg client (B.tail msg)
            n -> void $ forkIO $ handleGameOp n msg client gameReg
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

        let client = newClient newUuid conn
        let disconnect = do
                clients <- readTVarIO clientReg
                case clients !? newUuid of
                    Just client'' -> void $ removeFromGame client'' gameReg
                    _             -> pure ()
                modifyTVarIO clientReg (Map.delete newUuid)

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
    gameReg   <- atomically $ newTVar Map.empty
    httpServe (setPort 3000 mempty) (site clientReg gameReg)
