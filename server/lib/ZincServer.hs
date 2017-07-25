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
    ( -- * Data/type definitions
      Client
    , Key
    , Input
    , Color
    , PlayerState
    , GameState
    , Game
    , GameRegistry
    , ClientRegistry
    , -- * Constants
      hello
    , -- * Utility functions
      ($)
    , (*+)
    , (?)
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
    , ( #= )
    , (<#>)
    , ( # )
    , -- * Parsing functions
      parseByteString
    , parseColor
    , parseInput
    , -- * Primary functions
      sendGameList
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
import           Control.Concurrent            (forkIO, threadDelay)
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

import           Prelude                       hiding (return, ($))

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
    {-# INLINE (==) #-}

instance Ord Client where
    c1 <  c2 = (c1^.uuid) <  (c2^.uuid)
    {-# INLINE (<) #-}
    c1 <= c2 = (c1^.uuid) <= (c2^.uuid)
    {-# INLINE (<=) #-}
    c1 >  c2 = (c1^.uuid) >  (c2^.uuid)
    {-# INLINE (>) #-}
    c1 >= c2 = (c1^.uuid) >= (c2^.uuid)
    {-# INLINE (>=) #-}

instance Show Client where
    show c = "Client {"
          ++ UID.toString (c^.uuid)
          ++ ", "
          ++ BC.unpack (c^.username)
          ++ "}"
    {-# INLINE show #-}

type Key = V2 Double

data Input = Input
    { _ordinal   :: Word32
    , _timeStamp :: Double
    , _key       :: Key
    , _isDown    :: Bool
    } deriving (Eq, Show)

instance Ord Input where
    i1 <  i2 = (i1^.ordinal) <  (i2^.ordinal)
    {-# INLINE (<) #-}
    i1 <= i2 = (i1^.ordinal) <= (i2^.ordinal)
    {-# INLINE (<=) #-}
    i1 >  i2 = (i1^.ordinal) >  (i2^.ordinal)
    {-# INLINE (>) #-}
    i1 >= i2 = (i1^.ordinal) >= (i2^.ordinal)
    {-# INLINE (>=) #-}

data Color = Color Word8 Word8 Word8
    deriving (Eq, Show)

instance Functor Color where
    fmap f (Color r g b) = Color (f r) (f g) (f b)
    {-# INLINE fmap #-}
    (<$) a _ = Color a a a
    {-# INLINE (<$) #-}

instance Foldable Color where
    foldMap f (Color r g b) = f r *+ f g *+ f b
    {-# INLINE foldMap #-}

instance Num Color where
    (+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
    {-# INLINE (+) #-}
    (-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
    {-# INLINE (-) #-}
    (*) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)
    {-# INLINE (*) #-}
    abs = fmap signum
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}
    negate = fmap negate
    {-# INLINE negate #-}

instance Integral Color where
    quot (Color r1 g1 b1) (Color r2 g2 b2) = Color (quot r1 r2)
                                                   (quot g1 g2)
                                                   (quot b1 b2)
    {-# INLINE quot #-}
    rem (Color r1 g1 b1) (Color r2 g2 b2) = Color (rem r1 r2)
                                                  (rem g1 g2)
                                                  (rem b1 b2)
    {-# INLINE rem #-}
    div (Color r1 g1 b1) (Color r2 g2 b2) = Color (div r1 r2)
                                                  (div g1 g2)
                                                  (div b1 b2)
    {-# INLINE div #-}
    mod (Color r1 g1 b1) (Color r2 g2 b2) = Color (mod r1 r2)
                                                  (mod g1 g2)
                                                  (mod b1 b2)
    {-# INLINE mod #-}
    quotRem (Color r1 g1 b1) (Color r2 g2 b2) =
        let (rq, rr) = quotRem r1 r2
            (gq, gr) = quotRem g1 g2
            (bq, br) = quotRem b1 b2
        in  (Color rq gq bq, Color rr gr br)
    {-# INLINE quotRem #-}
    toInteger (Color r g b) =
        let (Right w32, _) = BG.runGet BG.getWord32le (B.pack [r, g, b, 0])
        in  toInteger w32
    {-# INLINE toInteger #-}

instance Real Color where
    toRational = fmap toRational
    {-# INLINE toRational #-}

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

type GameRegistry = Map ByteString (TVar Game)

type ClientRegistry = Map UUID Client


{- Lenses -}
uuid :: Lens' Client UUID
uuid = lens _uuid (\c uu -> c { _uuid = uu })

username :: Lens' Client ByteString
username = lens _username (\c un -> c { _username = un })

connection :: Lens' Client WS.Connection
connection = lens _connection (\c cn -> c { _connection = cn })

currGame :: Lens' Client ByteString
currGame = lens _currGame (\c cg -> c { _currGame = cg })

ordinal :: Lens' Input Int
ordinal = lens _ordinal (\i o -> i { _ordinal = o })

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

-- * Constants

hello :: ByteString
hello = "\x05\x58\xE6\x39\x0A\xC4\x13\x24"

-- * Utility functions

-- | Replacing '$' with '$!'.
infixr 0 $
($) :: (a -> b) -> a -> b
($) = ($!)
{-# INLINE ($) #-}

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

numClients :: TVar ClientRegistry -> IO Int
numClients clientReg = readTVarIO clientReg >>= pure . Map.size
{-# INLINE numClients #-}

clientExists :: Client -> TVar ClientRegistry -> IO Bool
clientExists client clientReg =
    readTVarIO clientReg >>= pure . Map.member (client^.uuid)
{-# INLINE clientExists #-}

isInGame :: Client -> Bool
isInGame client = not $ B.null (client^.currGame)
{-# INLINE isInGame #-}

addClient :: Client -> ClientRegistry -> ClientRegistry
addClient client = Map.insert (client^.uuid) client
{-# INLINE addClient #-}

removeFromGame :: Client -> TVar GameRegistry -> STM ()
removeFromGame client gameReg = do
    let name = client^.currGame
    games <- readTVar gameReg
    case games !? name of
        Just game' -> do
            game <- readTVar game'
            if Map.size (game^.players) < 2 then
                -- We're removing the last player, so just get rid of the
                -- whole game.
                modifyTVar' gameReg (Map.delete name)
            else
                -- Just remove the client from the game's player set.
                modifyTVar' game' (& players %~ Map.delete client)
        _ -> pure ()
{-# INLINE removeFromGame #-}

createNewGame :: Client ->
                 ByteString ->
                 TVar ClientRegistry ->
                 TVar GameRegistry ->
                 STM (TVar Game)
createNewGame host name clientReg gameReg = do
    let host' = host & currGame .~ name
    removeFromGame host' gameReg
    modifyTVar' clientReg (addClient host')
    game <- newTVar $ newGame name host'
    modifyTVar' gameReg (Map.insert name game)
    pure game

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

parseInput :: ByteString -> Maybe Input
parseInput s =
    case BG.runGet getInput s of
        (Right (# ordinal', timeStamp', key', isDown' #), "") ->
            Just $ Input
                { _ordinal   = ordinal'
                , _timeStamp = timeStamp'
                , _key       = getKey key'
                , _isDown    = isDown' /= 0
                }
        _ -> Nothing
  where
    getInput = do
        ordinal'   <- BG.getWord32le
        timeStamp' <- BG.getFloat64host
        key'       <- BG.getWord8
        isDown'    <- BG.getWord8
        pure (# ordinal', timeStamp', key', isDown' #)

    getKey k =
        case k of
            0 -> V2 0    (-1)
            1 -> V2 (-1) 0
            2 -> V2 0    1
            3 -> V2 1    0

-- * Primary functions

gameMainLoop :: TVar Game -> IO ()
gameMainLoop game = do
    forkIO $ physicsLoop
    forkIO $ broadcastLoop

sendGameList :: TVar GameRegistry -> Client -> IO ()
sendGameList gameReg client =
    let conn = client^.connection
        getGameInfo g' = do
            g <- readTVar g'
            pure $ B.cons (fromIntegral (B.length $ g^.gameName))
                 $ g^.gameName
                *+ B.singleton (fromIntegral $ Map.size $ g^.players)
    in  do
            games <- readTVarIO gameReg
            gameInfoList <- atomically (traverse getGameInfo (Map.elems games))
            WS.sendBinaryData conn (B.cons 0 $ B.concat gameInfoList)

registerNewGame :: TVar ClientRegistry ->
                   TVar GameRegistry ->
                   Client ->
                   ByteString ->
                   IO ()
registerNewGame clientReg gameReg host msg =
    if length parsed /= 2 || any invalidString parsed then
        WS.sendBinaryData conn ("\x01\x01" :: ByteString)
    else let [newUsername, newGameName] = (Seq.index parsed) <$> [0, 1] in
        if | B.length newUsername > 24 ->
                WS.sendBinaryData conn ("\x01\x02" :: ByteString)
           | B.length newGameName > 32 ->
                WS.sendBinaryData conn ("\x01\x03" :: ByteString)
           | otherwise -> do
                let sameUsername client = client^.username == newUsername
                clients <- readTVarIO clientReg
                games <- readTVarIO gameReg
                if | any sameUsername $ Map.elems clients ->
                        WS.sendBinaryData conn ("\x01\x02" :: ByteString)
                   | newGameName `Map.member` games ->
                        WS.sendBinaryData conn ("\x01\x03" :: ByteString)
                   | otherwise -> do
                        let host' = host & username .~ newUsername
                        game <- atomically $
                            createNewGame host' newGameName clientReg gameReg
                        gameMainLoop game
                        WS.sendBinaryData conn ("\x01\x00" :: ByteString)
  where
    conn = host^.connection
    parsed = parseByteString msg
    invalidString s = B.length s < 2 || B.any (\w -> w < 32 || w > 126) s

setGameInfo :: Client -> ByteString -> TVar Game -> IO ()
setGameInfo client msg game' =
    case parseColor msg of
        Just c  -> modifyTVarIO game' (client#color #= c)
        Nothing -> pure ()
{-# INLINE setGameInfo #-}

handleInputs :: Client -> ByteString -> TVar Game -> IO ()
handleInputs client msg game' =
    case parseInput msg of
        Just input -> modifyTVarIO game' (client#inputQueue <#> (|> input))
        Nothing    -> pure ()
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
            4 -> atomically $ removeFromGame client gameReg
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
    msg <- WS.receiveData conn

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
