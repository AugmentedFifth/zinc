{-# LANGUAGE Strict      #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module      : ZincData
--   Description : Data/type definitions, instances, and lenses for the /zinc/
--                     game.
--   Copyright   : [copyleft] AugmentedFifth, 2017
--   License     : AGPL-3
--   Maintainer  : zcomito@gmail.com
--   Stability   : experimental
--   Portability : POSIX
module ZincData where

import           Control.Concurrent            (ThreadId)
import           Control.Concurrent.STM
import           Control.Lens.Lens             (Lens', lens)

import           Data.Bits                     ((.&.))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Map.Strict               (Map)
import           Data.Ratio                    ((%))
import           Data.Sequence                 (Seq)
import qualified Data.Serialize.Get            as BG
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UID
import           Data.Word                     (Word32, Word8)

import           Linear.V2                     (V2)

import qualified Network.WebSockets.Connection as WS

import           Prelude                       hiding (pi, return, ($))


-- * Data/type definitions

data Client = Client
    { _uuid       :: UUID
    , _connection :: WS.Connection
    , _username   :: ByteString
    , _currGame   :: ByteString
    }

type Key = V2 Double

data Click = Click Double (# Double | V2 Double #)
    deriving (Eq, Ord, Show)

data Input = Input
    { _timeStamp :: Double
    , _key       :: Key
    , _isDown    :: Bool
    } deriving (Eq, Show)

data InputGroup = InputGroup
    { _ordinal :: Word32
    , _now     :: Double
    , _dt      :: Double
    , _inputs  :: Seq Input
    , _clicks  :: Seq Click
    } deriving (Eq, Show)

data Color = Color Word8 Word8 Word8
    deriving (Eq, Show)

data Projectile = Projectile
                  Word32      -- ^ ID
                  (V2 Double) -- ^ Position
                  (V2 Double) -- ^ Velocity
                  Word8       -- ^ Phase:
                              --   0 = normal flight,
                              --   1 = Broken but not broadcast,
                              --   2 = Broken and broadcast
    deriving (Eq, Show)

data PlayerState = PlayerState
    { _pos         :: V2 Double
    , _vel         :: V2 Double
    , _color       :: Color
    , _inputQueue  :: Seq InputGroup
    , _projectiles :: Seq Projectile
    , _lastOrdinal :: Word32
    } deriving (Show)

type GameState = Int

data Game = Game
    { _gameName        :: ByteString
    , _players         :: Map Client PlayerState
    , _gameState       :: GameState
    , _physicsLoopId   :: Maybe ThreadId
    , _broadcastLoopId :: Maybe ThreadId
    } deriving (Show)

type GameRegistry = Map ByteString (TVar Game)

type ClientRegistry = Map UUID Client


-- * Instances

instance Eq Client where
    (==) c1 c2 = _uuid c1 == _uuid c2
    {-# INLINE (==) #-}

instance Ord Client where
    c1 <  c2 = _uuid c1 <  _uuid c2
    {-# INLINE (<) #-}
    c1 <= c2 = _uuid c1 <= _uuid c2
    {-# INLINE (<=) #-}
    c1 >  c2 = _uuid c1 >  _uuid c2
    {-# INLINE (>) #-}
    c1 >= c2 = _uuid c1 >= _uuid c2
    {-# INLINE (>=) #-}

instance Show Client where
    show c = "Client {"
          ++ UID.toString (_uuid c)
          ++ ", "
          ++ BC.unpack (_username c)
          ++ "}"
    {-# INLINE show #-}

instance Ord InputGroup where
    i1 <  i2 = _ordinal i1 <  _ordinal i2
    {-# INLINE (<) #-}
    i1 <= i2 = _ordinal i1 <= _ordinal i2
    {-# INLINE (<=) #-}
    i1 >  i2 = _ordinal i1 >  _ordinal i2
    {-# INLINE (>) #-}
    i1 >= i2 = _ordinal i1 >= _ordinal i2
    {-# INLINE (>=) #-}

instance Num Color where
    (+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
    {-# INLINE (+) #-}
    (-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
    {-# INLINE (-) #-}
    (*) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)
    {-# INLINE (*) #-}
    abs (Color r g b) = Color (abs r) (abs g) (abs b)
    {-# INLINE abs #-}
    signum (Color r g b) = Color (signum r) (signum g) (signum b)
    {-# INLINE signum #-}
    fromInteger i = Color (fromIntegral $! i .&. 0x0000FF)
                          (fromIntegral $! i .&. 0x00FF00)
                          (fromIntegral $! i .&. 0xFF0000)
    {-# INLINE fromInteger #-}
    negate (Color r g b) = Color (negate r) (negate g) (negate b)
    {-# INLINE negate #-}

instance Enum Color where
    fromEnum (Color r g b) =
        let Right w32 = BG.runGet BG.getWord32le (B.pack [r, g, b, 0])
        in  fromIntegral w32
    {-# INLINE fromEnum #-}
    toEnum i = Color (fromIntegral $! i .&. 0x0000FF)
                     (fromIntegral $! i .&. 0x00FF00)
                     (fromIntegral $! i .&. 0xFF0000)
    {-# INLINE toEnum #-}

instance Ord Color where
    c1 <  c2 = fromEnum c1 <  fromEnum c2
    {-# INLINE (<)  #-}
    c1 <= c2 = fromEnum c1 <= fromEnum c2
    {-# INLINE (<=) #-}
    c1 >  c2 = fromEnum c1 >  fromEnum c2
    {-# INLINE (>)  #-}
    c1 >= c2 = fromEnum c1 >= fromEnum c2
    {-# INLINE (>=) #-}

instance Integral Color where
    quot (Color r1 g1 b1) (Color r2 g2 b2) = Color (quot r1 r2)
                                                   (quot g1 g2)
                                                   (quot b1 b2)
    {-# INLINE quot #-}
    rem  (Color r1 g1 b1) (Color r2 g2 b2) = Color (rem  r1 r2)
                                                   (rem  g1 g2)
                                                   (rem  b1 b2)
    {-# INLINE rem #-}
    div  (Color r1 g1 b1) (Color r2 g2 b2) = Color (div  r1 r2)
                                                   (div  g1 g2)
                                                   (div  b1 b2)
    {-# INLINE div #-}
    mod  (Color r1 g1 b1) (Color r2 g2 b2) = Color (mod  r1 r2)
                                                   (mod  g1 g2)
                                                   (mod  b1 b2)
    {-# INLINE mod #-}
    quotRem (Color r1 g1 b1) (Color r2 g2 b2) =
        let (rq, rr) = quotRem r1 r2
            (gq, gr) = quotRem g1 g2
            (bq, br) = quotRem b1 b2
        in  (Color rq gq bq, Color rr gr br)
    {-# INLINE quotRem #-}
    toInteger (Color r g b) =
        let Right w32 = BG.runGet BG.getWord32le (B.pack [r, g, b, 0])
        in  toInteger w32
    {-# INLINE toInteger #-}

instance Real Color where
    toRational c = 1 % toInteger c
    {-# INLINE toRational #-}


-- * Lenses

uuid :: Lens' Client UUID
uuid = lens _uuid (\c uu -> c { _uuid = uu })
{-# INLINE uuid #-}

connection :: Lens' Client WS.Connection
connection = lens _connection (\c cn -> c { _connection = cn })
{-# INLINE connection #-}

username :: Lens' Client ByteString
username = lens _username (\c un -> c { _username = un })
{-# INLINE username #-}

currGame :: Lens' Client ByteString
currGame = lens _currGame (\c cg -> c { _currGame = cg })
{-# INLINE currGame #-}

timeStamp :: Lens' Input Double
timeStamp = lens _timeStamp (\i ts -> i { _timeStamp = ts })
{-# INLINE timeStamp #-}

key :: Lens' Input Key
key = lens _key (\i k -> i { _key = k })
{-# INLINE key #-}

isDown :: Lens' Input Bool
isDown = lens _isDown (\i d -> i { _isDown = d })
{-# INLINE isDown #-}

ordinal :: Lens' InputGroup Word32
ordinal = lens _ordinal (\ig o -> ig { _ordinal = o })
{-# INLINE ordinal #-}

now :: Lens' InputGroup Double
now = lens _now (\ig n -> ig { _now = n })
{-# INLINE now #-}

dt :: Lens' InputGroup Double
dt = lens _dt (\ig t -> ig { _dt = t })
{-# INLINE dt #-}

inputs :: Lens' InputGroup (Seq Input)
inputs = lens _inputs (\ig is -> ig { _inputs = is })
{-# INLINE inputs #-}

clicks :: Lens' InputGroup (Seq Click)
clicks = lens _clicks (\ig cs -> ig { _clicks = cs })
{-# INLINE clicks #-}

pos :: Lens' PlayerState (V2 Double)
pos = lens _pos (\ps p -> ps { _pos = p })
{-# INLINE pos #-}

vel :: Lens' PlayerState (V2 Double)
vel = lens _vel (\ps v -> ps { _vel = v })
{-# INLINE vel #-}

color :: Lens' PlayerState Color
color = lens _color (\ps c -> ps { _color = c })
{-# INLINE color #-}

inputQueue :: Lens' PlayerState (Seq InputGroup)
inputQueue = lens _inputQueue (\ps iq -> ps { _inputQueue = iq })
{-# INLINE inputQueue #-}

projectiles :: Lens' PlayerState (Seq Projectile)
projectiles = lens _projectiles (\ps pj -> ps { _projectiles = pj })
{-# INLINE projectiles #-}

lastOrdinal :: Lens' PlayerState Word32
lastOrdinal = lens _lastOrdinal (\ps lo -> ps { _lastOrdinal = lo })
{-# INLINE lastOrdinal #-}

gameName :: Lens' Game ByteString
gameName = lens _gameName (\g gn -> g { _gameName = gn })
{-# INLINE gameName #-}

players :: Lens' Game (Map Client PlayerState)
players = lens _players (\g ps -> g { _players = ps })
{-# INLINE players #-}

gameState :: Lens' Game GameState
gameState = lens _gameState (\g gs -> g { _gameState = gs })
{-# INLINE gameState #-}

physicsLoopId :: Lens' Game (Maybe ThreadId)
physicsLoopId = lens _physicsLoopId (\g pi -> g { _physicsLoopId = pi })
{-# INLINE physicsLoopId #-}

broadcastLoopId :: Lens' Game (Maybe ThreadId)
broadcastLoopId = lens _broadcastLoopId (\g bi -> g { _broadcastLoopId = bi })
{-# INLINE broadcastLoopId #-}
