module Types where

import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Lens (Lens', lens, (^.))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Graphics.Canvas (CanvasImageSource)
import Graphics.Drawing (Point)
import Prelude
import Signal.DOM (DimensionPair)
import Signal.Touch (TouchEvent_)
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)

type Rect  = { x :: Number, y :: Number, w :: Number, h :: Number }
newtype ScreenPoint = ScreenPoint Point
newtype MapPoint = MapPoint { x :: Int, y :: Int }

type GAssets v = {
    playerGreen :: v,
    playerYellow :: v,
    playerRed :: v,
    playerPurple :: v,
    exploreGreenN :: v,
    exploreGreenE :: v,
    exploreGreenS :: v,
    exploreGreenW :: v,
    exploreYellowN :: v,
    exploreYellowE :: v,
    exploreYellowS :: v,
    exploreYellowW :: v,
    exploreRedN :: v,
    exploreRedE :: v,
    exploreRedS :: v,
    exploreRedW :: v,
    explorePurpleN :: v,
    explorePurpleE :: v,
    explorePurpleS :: v,
    explorePurpleW :: v,
    wallRight :: v,
    wallDown :: v,
    wallRightDown :: v,
    wallNWCorner :: v,
    warpGreen :: v,
    warpYellow :: v,
    warpRed :: v,
    warpPurple :: v,
    exitPurpleN :: v,
    exitPurpleE :: v,
    exitPurpleS :: v,
    exitPurpleW :: v,
    weaponGreen :: v,
    weaponYellow :: v,
    weaponRed :: v,
    weaponPurple :: v,
    hourglassRed :: v,
    hourglassBlack :: v,
    overlay :: v,
    cardN :: v,
    cardE :: v,
    cardS :: v,
    cardW :: v,
    nametag :: v,
    cellTop :: v,
    cellBottomRight :: v,
    cellBottomLeft :: v,
    background :: v
  }

data AssetName =
    APlayer PlayerColor
  | AExplore PlayerColor Dir
  | AWallRight
  | AWallDown
  | AWallRightDown
  | AWallNWCorner
  | AWarp PlayerColor
  | AExit Dir -- Eventually this will have a PlayerColor index as well
  | AWeapon PlayerColor
  | AHourglassRed
  | AHourglassBlack
  | ABackground
  | AOverlay
  | ACard Dir
  | ANametag
  | ACellTop
  | ACellBottomRight
  | ACellBottomLeft

assetLookup :: forall v. AssetName -> GAssets v -> v
assetLookup (APlayer Green) r = r.playerGreen
assetLookup (APlayer Yellow) r = r.playerYellow
assetLookup (APlayer Red) r = r.playerRed
assetLookup (APlayer Purple) r = r.playerPurple
assetLookup (AExplore Green N) r = r.exploreGreenN
assetLookup (AExplore Green E) r = r.exploreGreenE
assetLookup (AExplore Green S) r = r.exploreGreenS
assetLookup (AExplore Green W) r = r.exploreGreenW
assetLookup (AExplore Yellow N) r = r.exploreYellowN
assetLookup (AExplore Yellow E) r = r.exploreYellowE
assetLookup (AExplore Yellow S) r = r.exploreYellowS
assetLookup (AExplore Yellow W) r = r.exploreYellowW
assetLookup (AExplore Red N) r = r.exploreRedN
assetLookup (AExplore Red E) r = r.exploreRedE
assetLookup (AExplore Red S) r = r.exploreRedS
assetLookup (AExplore Red W) r = r.exploreRedW
assetLookup (AExplore Purple N) r = r.explorePurpleN
assetLookup (AExplore Purple E) r = r.explorePurpleE
assetLookup (AExplore Purple S) r = r.explorePurpleS
assetLookup (AExplore Purple W) r = r.explorePurpleW
assetLookup AWallRight r = r.wallRight
assetLookup AWallDown r = r.wallDown
assetLookup AWallRightDown r = r.wallRightDown
assetLookup AWallNWCorner r = r.wallNWCorner
assetLookup (AWarp Green) r = r.warpGreen
assetLookup (AWarp Yellow) r = r.warpYellow
assetLookup (AWarp Red) r = r.warpRed
assetLookup (AWarp Purple) r = r.warpPurple
assetLookup (AExit N) r = r.exitPurpleN
assetLookup (AExit E) r = r.exitPurpleE
assetLookup (AExit S) r = r.exitPurpleS
assetLookup (AExit W) r = r.exitPurpleW
assetLookup (AWeapon Green) r = r.weaponGreen
assetLookup (AWeapon Yellow) r = r.weaponYellow
assetLookup (AWeapon Red) r = r.weaponRed
assetLookup (AWeapon Purple) r = r.weaponPurple
assetLookup AHourglassRed r = r.hourglassRed
assetLookup AHourglassBlack r = r.hourglassBlack
assetLookup AOverlay r = r.overlay
assetLookup (ACard N) r = r.cardN
assetLookup (ACard E) r = r.cardE
assetLookup (ACard S) r = r.cardS
assetLookup (ACard W) r = r.cardW
assetLookup ANametag r = r.nametag
assetLookup ACellTop r = r.cellTop
assetLookup ACellBottomRight r = r.cellBottomRight
assetLookup ACellBottomLeft r = r.cellBottomLeft
assetLookup ABackground r = r.background

type Assets = GAssets CanvasImageSource

newtype DirMap v = DirMap { left :: v, up :: v, right :: v, down :: v }
data Dir =
    N
  | E
  | S
  | W

data SpecialTile =
    STUnwalkable
  | STEntrance
  | STExplore PlayerColor Dir
  | STWarp PlayerColor
  | STExit PlayerColor Dir
  | STWeapon PlayerColor
  | STTimer Boolean -- isActive

data Escalator = Escalator MapPoint MapPoint
newtype Entrance = Entrance { side :: Dir, offset :: Int }
newtype Walls = Walls { right :: Boolean, down :: Boolean  }
newtype Cell = Cell { walls :: Walls, special :: Maybe SpecialTile }
type Cells = Map MapPoint Cell

-- TODO Set Escalator after adding Encode (Set v)
newtype Maze = Maze { cells :: Cells, borders :: DirMap Int, escalators :: Array Escalator, wepacq :: Boolean }
newtype Tile = Tile { cells :: Cells, entrance :: Entrance, escalators :: Array Escalator }

data GameStatus =
    Waiting
  | Started
  | WeaponsAcquired
  | Won
  | Lost
  | RoomFull

type PlayerPositions = Map PlayerColor MapPoint
type DragState = { playerColor :: PlayerColor, dragPoint :: Point }
type TouchState = { pagePoint :: Point, renderOffset :: Point }
type GameState = {
  maze :: Maze,
  tiles :: Array Tile,
  players :: PlayerPositions,
  dragging :: Maybe DragState,
  renderOffset :: Point,
  timer :: Int,
  status :: GameStatus,
  allowedDir :: Dir,
  clients :: Map String Dir,
  touches :: Map Int TouchState
  }

-- CLEANUP GameState = { sgs :: ServerGameState } get rid of the duplication. Similar cleanup for Tile and Maze ?
newtype ServerGameState = ServerGameState {
  maze :: Maze,
  tiles :: Array Tile,
  players :: PlayerPositions,
  timer :: Int,
  status :: GameStatus
  }

type RealMouseInputs =
  { offscreenDims :: DimensionPair, realMousePos :: Point, mousePressed :: Boolean, ws :: Maybe (WebSocket) }
type MouseInputs =
  { offscreenDims :: DimensionPair, mousePos :: Point, mousePressed :: Boolean, ws :: Maybe (WebSocket) }
type KeyboardInputs =
  { screenDims :: { w :: Number, h :: Number }, offscreenDims :: DimensionPair, up :: Boolean, down :: Boolean, left :: Boolean, right :: Boolean }
type MouseWheelInputs =
  { screenDims :: { w :: Number, h :: Number }, offscreenDims :: DimensionPair, mWheelEvent :: (Maybe WheelEvent) }
type TouchInputs =
  { screenDims :: { w :: Number, h :: Number }, offscreenDims :: DimensionPair, mTouchEvent :: (Maybe TouchEvent_) }
data Inputs =
    Mouse MouseInputs
  | Keyboard KeyboardInputs
  | MouseWheel MouseWheelInputs
  | Touch TouchInputs
  | ServerMsg (Maybe String)
  | Tick

data C2SCommand =
    CPlayerMove PlayerColor MapPoint MapPoint  -- From To
  | CExplore MapPoint Dir

data S2CCommand =
    SPlayerMove PlayerColor MapPoint
  | SExplore Int MapPoint Dir
  | SSetState ServerGameState
  | SSetAllowedDir Dir
  | SSetClients (Map String Dir)
  | SRoomFull

data PlayerColor =
    Red
  | Yellow
  | Green
  | Purple

-- TODO rewrite as traversal ?
forAllCells :: forall m. Monoid m => Maze -> (Int -> Int -> Cell -> m) -> m
forAllCells maze f =
  foldMap
    (\x ->
      foldMap
        (\y ->
          maybe
            mempty
            (f x y)
            (lookup (MapPoint { x, y }) $ maze^.cells))
        ((maze^.borders^.up) .. (maze^.borders^.down)))
    ((maze^.borders^.left) .. (maze^.borders^.right))

toPoint :: { x :: Int, y :: Int } -> Point
toPoint { x, y } = { x: toNumber x, y: toNumber y }

toScreenPoint :: { x :: Int, y :: Int } -> ScreenPoint
toScreenPoint { x, y } = ScreenPoint { x: toNumber x, y: toNumber y }

-- Lenses
serverGameState :: Lens' GameState ServerGameState
serverGameState = lens toSGS setFromSGS
  where
    toSGS gs = ServerGameState { maze: gs.maze, tiles: gs.tiles, players: gs.players, timer: gs.timer, status: gs.status }
    -- We set dragging to Nothing when the server explicitly sends us a state. Because this only happens when the game starts.
    setFromSGS gs (ServerGameState sgs) =
      gs { maze = sgs.maze, players = sgs.players, tiles = sgs.tiles, timer = sgs.timer, status = sgs.status, dragging = Nothing }

left :: forall v. Lens' (DirMap v) v
left = lens (_.left <<< unwrap) $ \s b -> wrap $ _ { left = b } $ unwrap s

up :: forall v. Lens' (DirMap v) v
up = lens (_.up <<< unwrap) $ \s b -> wrap $ _ { up = b } $ unwrap s

right :: forall v n r. Newtype n { right :: v | r } => Lens' n v
right = lens (_.right <<< unwrap) $ \s b -> wrap $ _ { right = b } $ unwrap s

down :: forall v n r. Newtype n { down :: v | r } => Lens' n v
down = lens (_.down <<< unwrap) $ \s b -> wrap $ _ { down = b } $ unwrap s

cells :: forall n r. Newtype n { cells :: Cells | r } => Lens' n Cells
cells = lens (_.cells <<< unwrap) $ \s b -> wrap $ _ { cells = b } $ unwrap s

wepacq :: forall n r. Newtype n { wepacq :: Boolean | r } => Lens' n Boolean
wepacq = lens (_.wepacq <<< unwrap) $ \s b -> wrap $ _ { wepacq = b } $ unwrap s

borders :: Lens' Maze (DirMap Int)
borders = lens (_.borders <<< unwrap) $ \s b -> wrap $ _ { borders = b } $ unwrap s

escalators :: forall n r. Newtype n { escalators :: Array Escalator | r } => Lens' n (Array Escalator)
escalators = lens (_.escalators <<< unwrap) $ \s b -> wrap $ _ { escalators = b } $ unwrap s

side :: Lens' Entrance Dir
side = lens (_.side <<< unwrap) $ \s b -> wrap $ _ { side = b } $ unwrap s

offset :: Lens' Entrance Int
offset = lens (_.offset <<< unwrap) $ \s b -> wrap $ _ { offset = b } $ unwrap s

entrance :: Lens' Tile Entrance
entrance = lens (_.entrance <<< unwrap) $ \s b -> wrap $ _ { entrance = b } $ unwrap s

walls :: Lens' Cell Walls
walls = lens (_.walls <<< unwrap) $ \s b -> wrap $ _ { walls = b } $ unwrap s

special :: Lens' Cell (Maybe SpecialTile)
special = lens (_.special <<< unwrap) $ \s b -> wrap $ _ { special = b } $ unwrap s

-- eq, ord, show, semiring, ring, generic, encode, decode and newtype instances
derive instance newTypeDirMap :: Newtype (DirMap v) _
derive instance genericDirMap :: Generic (DirMap v) _
instance encodeDirMap :: (Encode v) => Encode (DirMap v) where
  encode = genericEncode defaultOptions
instance decodeDirMap :: (Decode v) => Decode (DirMap v) where
  decode = genericDecode defaultOptions

derive instance eqDir :: Eq Dir
derive instance ordDir :: Ord Dir
derive instance genericDir :: Generic Dir _
instance showDir :: Show Dir where show = genericShow
instance encodeDir :: Encode Dir where
  encode = genericEncode defaultOptions
instance decodeDir :: Decode Dir where
  decode = genericDecode defaultOptions

derive instance eqSpecialTile :: Eq SpecialTile
derive instance ordSpecialTile :: Ord SpecialTile
derive instance genericSpecialTile :: Generic SpecialTile _
instance showSpecialTile :: Show SpecialTile where show = genericShow
instance encodeSpecialTile :: Encode SpecialTile where
  encode = genericEncode defaultOptions
instance decodeSpecialTile :: Decode SpecialTile where
  decode = genericDecode defaultOptions

derive instance genericEscalator :: Generic Escalator _
derive instance eqEscalator :: Eq Escalator
derive instance ordEscalator :: Ord Escalator
instance encodeEscalator :: Encode Escalator where
  encode = genericEncode defaultOptions
instance decodeEscalator :: Decode Escalator where
  decode = genericDecode defaultOptions

derive instance newtypeMaze :: Newtype Maze _
derive instance genericMaze :: Generic Maze _
instance encodeMaze :: Encode Maze where
  encode = genericEncode defaultOptions
instance decodeMaze :: Decode Maze where
  decode = genericDecode defaultOptions

derive instance newtypeEntrance :: Newtype Entrance _
derive instance genericEntrance :: Generic Entrance _
instance encodeEntrance :: Encode Entrance where
  encode = genericEncode defaultOptions
instance decodeEntrance :: Decode Entrance where
  decode = genericDecode defaultOptions

derive instance newtypeTile :: Newtype Tile _
derive instance genericTile :: Generic Tile _
instance encodeTile :: Encode Tile where
  encode = genericEncode defaultOptions
instance decodeTile :: Decode Tile where
  decode = genericDecode defaultOptions

derive instance newtypeWalls :: Newtype Walls _
derive instance genericWalls :: Generic Walls _
instance encodeWalls :: Encode Walls where
  encode = genericEncode defaultOptions
instance decodeWalls :: Decode Walls where
  decode = genericDecode defaultOptions

derive instance newtypeCell :: Newtype Cell _
derive instance genericCell :: Generic Cell _
instance encodeCell :: Encode Cell where
  encode = genericEncode defaultOptions
instance decodeCell :: Decode Cell where
  decode = genericDecode defaultOptions

derive instance genericC2SCommand :: Generic C2SCommand _
derive instance genericS2CCommand :: Generic S2CCommand _

derive instance eqPlayerColor :: Eq PlayerColor
derive instance ordPlayerColor :: Ord PlayerColor
derive instance genericPlayerColor :: Generic PlayerColor _
instance showPlayerColor :: Show PlayerColor where show = genericShow
instance encodePlayerColor :: Encode PlayerColor where
  encode = genericEncode defaultOptions
instance decodePlayerColor :: Decode PlayerColor where
  decode = genericDecode defaultOptions

derive instance eqGameStatus :: Eq GameStatus
derive instance genericGameStatus :: Generic GameStatus _
instance showGameStatus :: Show GameStatus where
  show = genericShow
instance encodeGameStatus :: Encode GameStatus where
  encode = genericEncode defaultOptions
instance decodeGameStatus :: Decode GameStatus where
  decode = genericDecode defaultOptions

derive instance genericServerGameState :: Generic ServerGameState _
instance encodeServerGameState :: Encode ServerGameState where
  encode = genericEncode defaultOptions
instance decodeServerGameState :: Decode ServerGameState where
  decode = genericDecode defaultOptions
derive instance newtypeServerGameState :: Newtype ServerGameState _

derive instance newtypeScreenPoint :: Newtype ScreenPoint _
instance semiringScreenPoint :: Semiring ScreenPoint where
  add (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a + b)
  mul (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a * b)
  zero = ScreenPoint zero
  one = ScreenPoint one
instance ringScreenPoint :: Ring ScreenPoint where
  sub (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a - b)

derive instance eqMapPoint :: Eq MapPoint
derive instance ordMapPoint :: Ord MapPoint
instance semiringMapPoint :: Semiring MapPoint where
  add (MapPoint a) (MapPoint b) = MapPoint (a + b)
  mul (MapPoint a) (MapPoint b) = MapPoint (a * b)
  zero = MapPoint zero
  one = MapPoint one
instance ringMapPoint :: Ring MapPoint where
  sub (MapPoint a) (MapPoint b) = MapPoint (a - b)
derive instance genericMapPoint :: Generic MapPoint _
instance showMapPoint :: Show MapPoint where show = genericShow
instance encodeMapPoint :: Encode MapPoint where
  encode = genericEncode defaultOptions
instance decodeMapPoint :: Decode MapPoint where
  decode = genericDecode defaultOptions
