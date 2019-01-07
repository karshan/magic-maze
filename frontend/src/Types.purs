module Types where

import Prelude
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe (..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Foreign
import Foreign.Class (class Encode, class Decode, encode, decode)
import Foreign.Generic
import Foreign.Generic.Types
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Tuple (Tuple (..))
import Graphics.Drawing (Point)
import Graphics.Canvas (CanvasImageSource)
import Signal.DOM (DimensionPair, CoordinatePair)
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)

type Rect  = { x :: Number, y :: Number, w :: Number, h :: Number }

type DirMap v = { left :: v, up :: v, right :: v, down :: v }
newtype WDirMap v = DirMap { left :: v, up :: v, right :: v, down :: v }
derive instance newTypeDirMap :: Newtype (WDirMap v) _
derive instance genericDirMap :: Generic (WDirMap v) _
instance encodeDirMap :: (Encode v) => Encode (WDirMap v) where
  encode = genericEncode defaultOptions
instance decodeDirMap :: (Decode v) => Decode (WDirMap v) where
  decode = genericDecode defaultOptions

data Dir =
    N
  | E
  | S
  | W
derive instance eqDir :: Eq Dir
derive instance ordDir :: Ord Dir
derive instance genericDir :: Generic Dir _
instance showDir :: Show Dir where show = genericShow
instance encodeDir :: Encode Dir where
  encode = genericEncode defaultOptions
instance decodeDir :: Decode Dir where
  decode = genericDecode defaultOptions

data SpecialTile =
    STUnwalkable
  | STEntrance
  | STExplore PlayerColor Dir
  | STWarp PlayerColor
  | STExit PlayerColor Dir
  | STWeapon PlayerColor
  | STTimer
derive instance eqSpecialTile :: Eq SpecialTile
derive instance ordSpecialTile :: Ord SpecialTile
derive instance genericSpecialTile :: Generic SpecialTile _
instance showSpecialTile :: Show SpecialTile where show = genericShow
instance encodeSpecialTile :: Encode SpecialTile where
  encode = genericEncode defaultOptions
instance decodeSpecialTile :: Decode SpecialTile where
  decode = genericDecode defaultOptions

type Escalator = Tuple MapPoint MapPoint
data WEscalator = Escalator MapPoint MapPoint
derive instance genericEscalator :: Generic WEscalator _
instance encodeEscalator :: Encode WEscalator where
  encode = genericEncode defaultOptions
instance decodeEscalator :: Decode WEscalator where
  decode = genericDecode defaultOptions

type Cells = Map MapPoint Cell
type WCells = Map MapPoint WCell
type Maze = { cells :: Cells, borders :: DirMap Int, escalators :: Set Escalator }
newtype WMaze = Maze { cells :: WCells, borders :: WDirMap Int, escalators :: Array WEscalator }
derive instance newtypeMaze :: Newtype WMaze _
derive instance genericWMaze :: Generic WMaze _
instance encodeWMaze :: Encode WMaze where
  encode = genericEncode defaultOptions
instance decodeWMaze :: Decode WMaze where
  decode = genericDecode defaultOptions

newtype Person = Person { name :: String, age :: Number }
derive instance genericPerson :: Generic Person _
instance encodePerson :: Encode Person where
  encode = genericEncode defaultOptions
instance decodePerson :: Decode Person where
  decode = genericDecode defaultOptions

type Entrance = { side :: Dir, offset :: Int }
newtype WEntrance = Entrance Entrance
derive instance newtypeEntrance :: Newtype WEntrance _
derive instance genericEntrance :: Generic WEntrance _
instance encodeEntrance :: Encode WEntrance where
  encode = genericEncode defaultOptions
instance decodeEntrance :: Decode WEntrance where
  decode = genericDecode defaultOptions

type Tile = { cells :: Cells, entrance :: Entrance, escalators :: Set Escalator }
newtype WTile = Tile { cells :: WCells, entrance :: WEntrance, escalators :: Array WEscalator }
derive instance newtypeTile :: Newtype WTile _
derive instance genericTile :: Generic WTile _
instance encodeTile :: Encode WTile where
  encode = genericEncode defaultOptions
instance decodeTile :: Decode WTile where
  decode = genericDecode defaultOptions

type Walls = { right :: Boolean, down :: Boolean  }
newtype WWalls = Walls Walls
derive instance newtypeWalls :: Newtype WWalls _
derive instance genericWalls :: Generic WWalls _
instance encodeWalls :: Encode WWalls where
  encode = genericEncode defaultOptions
instance decodeWalls :: Decode WWalls where
  decode = genericDecode defaultOptions

type Cell = { walls :: Walls, special :: Maybe SpecialTile }
newtype WCell = Cell { walls :: WWalls, special :: Maybe SpecialTile }
derive instance newtypeCell :: Newtype WCell _
derive instance genericCell :: Generic WCell _
instance encodeCell :: Encode WCell where
  encode = genericEncode defaultOptions
instance decodeCell :: Decode WCell where
  decode = genericDecode defaultOptions

forAllCells :: forall m. Monoid m => Maze -> (Int -> Int -> Cell -> m) -> m
forAllCells maze f =
  foldMap
    (\x ->
      foldMap
        (\y ->
          maybe
            mempty
            (f x y)
            (lookup (MapPoint { x, y }) maze.cells))
        (maze.borders.up .. maze.borders.down))
    (maze.borders.left .. maze.borders.right)

type RealMouseInputs = 
  { offscreenDims :: DimensionPair, realMousePos :: Point, mousePressed :: Boolean, ws :: Maybe (WebSocket) }
type MouseInputs = 
  { offscreenDims :: DimensionPair, mousePos :: CoordinatePair, mousePressed :: Boolean, ws :: Maybe (WebSocket) }
type KeyboardInputs = 
  { offscreenDims :: DimensionPair, up :: Boolean, down :: Boolean, left :: Boolean, right :: Boolean,
    mouseWheel :: Maybe WheelEvent }
data Inputs =
    Mouse MouseInputs
  | Keyboard KeyboardInputs
  | ServerMsg (Maybe String)

-- TODO nonempty map like dirmap
type PlayerPositions = Map PlayerColor MapPoint
type GameState = { maze :: Maze, tiles :: Array Tile, players :: PlayerPositions, dragging :: Maybe DragState, renderOffset :: Point }
newtype ServerGameState = ServerGameState { maze :: WMaze, tiles :: Array WTile, players :: PlayerPositions }
type DragState = { playerColor :: PlayerColor, dragPoint :: Point }
derive instance genericServerGameState :: Generic ServerGameState _
instance encodeServerGameState :: Encode ServerGameState where
  encode = genericEncode defaultOptions
instance decodeServerGameState :: Decode ServerGameState where
  decode = genericDecode defaultOptions
derive instance newtypeServerGameState :: Newtype ServerGameState _

setSGS :: ServerGameState -> GameState -> GameState
setSGS sgs gs = gs { maze = fromWMaze $ (unwrap sgs).maze, players = gs.players, tiles = gs.tiles }

fromWMaze :: WMaze -> Maze
fromWMaze (Maze m) = { cells: fromWCells m.cells, borders: unwrap m.borders, escalators: fromWEscalators m.escalators }

fromWEscalators :: Array WEscalator -> Set Escalator
fromWEscalators es = Set.fromFoldable $ map (\(Escalator a b) -> Tuple a b) es

fromWCells :: Map MapPoint WCell -> Map MapPoint Cell
fromWCells = map (\(Cell c) -> { walls: (unwrap c.walls), special: c.special })

toSGS :: GameState -> ServerGameState
toSGS gs = ServerGameState { maze: toWMaze gs.maze, tiles: toWTiles gs.tiles, players: gs.players } -- maze: (toWMaze gs.maze), tiles: gs.tiles, players: gs.players }

toWTiles :: Array Tile -> Array WTile
toWTiles = map (\t -> Tile { cells: toWCells t.cells, entrance: Entrance t.entrance, escalators: toWEscalators t.escalators })

toWCell :: Cell -> WCell
toWCell cell = Cell { walls: Walls cell.walls, special: cell.special }

toWCells :: Cells -> WCells
toWCells = map toWCell

toWEscalators :: Set Escalator -> Array WEscalator
toWEscalators es = map (\(Tuple a b) -> Escalator a b) $ Set.toUnfoldable es

toWMaze :: Maze -> WMaze
toWMaze maze = Maze { cells: toWCells maze.cells, borders: DirMap maze.borders, escalators: toWEscalators maze.escalators }

data Command =
    PlayerMove PlayerColor MapPoint
  | Explore MapPoint Dir
  | SetState ServerGameState
derive instance genericCommand :: Generic Command _

data PlayerColor =
    Red
  | Yellow
  | Green
  | Purple
derive instance eqPlayerColor :: Eq PlayerColor
derive instance ordPlayerColor :: Ord PlayerColor
derive instance genericPlayerColor :: Generic PlayerColor _
instance showPlayerColor :: Show PlayerColor where show = genericShow
instance encodePlayerColor :: Encode PlayerColor where
  encode = genericEncode defaultOptions
instance decodePlayerColor :: Decode PlayerColor where
  decode = genericDecode defaultOptions

toPoint :: { x :: Int, y :: Int } -> Point
toPoint { x, y } = { x: toNumber x, y: toNumber y }

newtype ScreenPoint = ScreenPoint Point
derive instance newtypeScreenPoint :: Newtype ScreenPoint _
instance semiringScreenPoint :: Semiring ScreenPoint where
  add (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a + b)
  mul (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a * b)
  zero = ScreenPoint zero
  one = ScreenPoint one
instance ringScreenPoint :: Ring ScreenPoint where
  sub (ScreenPoint a) (ScreenPoint b) = ScreenPoint (a - b)

toScreenPoint :: { x :: Int, y :: Int } -> ScreenPoint
toScreenPoint { x, y } = ScreenPoint { x: toNumber x, y: toNumber y }

newtype MapPoint = MapPoint { x :: Int, y :: Int }
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

data AssetName =
    APlayer PlayerColor
  | AExplore PlayerColor
  | AWarp PlayerColor
  | AExit PlayerColor
  | AWeapon PlayerColor
  | ABackground
derive instance eqAssetName :: Eq AssetName
derive instance ordAssetName :: Ord AssetName
derive instance genericAssetName :: Generic AssetName _
instance showAssetName :: Show AssetName where show = genericShow

type Asset = Tuple AssetName (Maybe CanvasImageSource)
type Assets = Map AssetName CanvasImageSource
