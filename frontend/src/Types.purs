module Types where

import Prelude
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (lookup)
import Data.Maybe (Maybe (..), maybe)
import Foreign
import Foreign.Class
import Foreign.Generic
import Foreign.Generic.Types
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Graphics.Drawing (Point)
import Graphics.Canvas (CanvasImageSource)
import Signal.DOM (DimensionPair, CoordinatePair)
import Web.Socket.WebSocket (WebSocket)

type Rect  = { x :: Number, y :: Number, w :: Number, h :: Number }

type DirMap v = { left :: v, up :: v, right :: v, down :: v }

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
derive instance eqSpecialTile :: Eq SpecialTile
derive instance ordSpecialTile :: Ord SpecialTile
derive instance genericSpecialTile :: Generic SpecialTile _
instance showSpecialTile :: Show SpecialTile where show = genericShow
instance encodeSpecialTile :: Encode SpecialTile where
  encode = genericEncode defaultOptions
instance decodeSpecialTile :: Decode SpecialTile where
  decode = genericDecode defaultOptions

type Cells = Map MapPoint Cell
type Maze = { cells :: Cells, borders :: DirMap Int }
type Entrance = { side :: Dir, offset :: Int }
type Tile = { cells :: Cells, entrance :: Entrance }
type Cell = { walls :: { right :: Boolean, down :: Boolean }, special :: Maybe SpecialTile }

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

type MouseInputs = { dims :: DimensionPair, mousePos :: CoordinatePair, mousePressed :: Boolean, ws :: Maybe (WebSocket) }
data Inputs =
    Mouse MouseInputs
  | ServerMsg (Maybe String)

-- TODO nonempty map like dirmap
type PlayerPositions = Map PlayerColor MapPoint
type GameState = { maze :: Maze, players :: PlayerPositions, dragging :: Maybe DragState }
type DragState = { playerColor :: PlayerColor, dragPoint :: Point }

data Command =
    PlayerMove PlayerColor MapPoint
  | Explore MapPoint Dir
derive instance eqCommand :: Eq Command
derive instance ordCommand :: Ord Command
derive instance genericCommand :: Generic Command _
instance showComand :: Show Command where show = genericShow

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
  | ABackground
derive instance eqAssetName :: Eq AssetName
derive instance ordAssetName :: Ord AssetName
derive instance genericAssetName :: Generic AssetName _
instance showAssetName :: Show AssetName where show = genericShow

type Asset = Tuple AssetName (Maybe CanvasImageSource)
type Assets = Map AssetName CanvasImageSource
