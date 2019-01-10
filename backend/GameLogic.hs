{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GameLogic where

import Protolude
import Types

import Control.Monad.Random
import Control.Lens
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lenses as L

weaponsAcquired :: ServerGameState -> Bool
weaponsAcquired gs = 
    getAll $ Map.foldMapWithKey 
        (\pCol mp -> 
            maybe (All False) 
                (\cell -> All $ cell ^. L.special == Just (STWeapon pCol)) 
                (Map.lookup mp $ gs ^. L.maze.L.cells))
        (gs ^. L.players)

-- FIXME send revert commands on invalid commands
-- FIXME check from value
evalCommand :: (RandomGen g) => C2SCommand -> ServerGameState -> Rand g (ServerGameState, Maybe S2CCommand)
evalCommand (CPlayerMove pCol from to) sgs =
    if isValidMove pCol to sgs then do
        let serverCommand = SPlayerMove pCol to
            newGameState = sgs & over L.players (Map.adjust (const to) pCol)
        maybe
            (return (newGameState, Just serverCommand))
            (\cell ->
                case cell ^. L.special of
                    Just (STTimer True) ->
                        return (newGameState &
                            L.maze.L.cells %~ 
                                (Map.adjust (L.special .~ (Just $ STTimer False)) to) &
                            L.timer %~ (150 -), Just serverCommand)
                    Just (STWeapon _) ->
                        if weaponsAcquired newGameState then
                            return (newGameState & L.status .~ WeaponsAcquired, Just serverCommand)
                        else
                            return (newGameState, Just serverCommand)
                    Just (STExit _ _) ->
                        if newGameState ^. L.status == WeaponsAcquired then
                            if length (newGameState ^. L.players) <= 1 then
                                return (newGameState & L.status .~ Won & L.players .~ Map.empty, Just serverCommand)
                            else
                                return (newGameState & L.players %~ (Map.delete pCol), Just serverCommand)
                        else
                            return (newGameState, Just serverCommand)
                    _ ->
                        return (newGameState, Just serverCommand))
            (Map.lookup to (newGameState ^. L.maze.L.cells))
    else
        return (sgs, SPlayerMove pCol <$> (Map.lookup pCol (sgs ^. L.players)))
evalCommand (CExplore mp dir) sgs = do
    nextTile <- getRandomR (0, (length $ sgs ^. L.tiles) - 1)
    let serverCommand = SExplore nextTile mp dir
    maybe (return (sgs, Nothing))
        (\newMaze -> return (sgs & L.maze .~ newMaze & L.tiles .~ (deleteAt nextTile (sgs^.L.tiles)), Just serverCommand))
        (((sgs ^. L.tiles) !! nextTile) >>= (\newTile -> mergeTiles (sgs^.L.maze) newTile mp dir))

deleteAt = deleteAtH []

deleteAtH :: [a] -> Int -> [a] -> [a]
deleteAtH acc _ [] = acc
deleteAtH acc 0 (x:xs) = (acc ++ xs)
deleteAtH acc n (x:xs) = deleteAtH (acc ++ [x]) (n - 1) xs

(!!) :: [a] -> Int -> Maybe a
[] !! _ = Nothing
(x:xs) !! 0 = Just x
(x:xs) !! n = xs !! (n - 1)

blockedByWall :: Maze -> MapPoint -> MapPoint -> Dir -> Bool
blockedByWall maze (MapPoint cx cy) (MapPoint tx ty) dir =
  case dir of
    N -> any (\y -> maybe True (\(c :: Cell) -> c^.L.walls^.L.down) $ Map.lookup (MapPoint cx y) (maze^.L.cells))
            [ty..(cy - 1)]
    S -> any (\y -> maybe True (\(c :: Cell) -> c^.L.walls^.L.down) $ Map.lookup (MapPoint cx y) (maze^.L.cells))
            [cy..(ty - 1)]
    W -> any (\x -> maybe True (\(c :: Cell) -> c^.L.walls^.L.right) $ Map.lookup (MapPoint x cy) (maze^.L.cells))
            [tx..(cx - 1)]
    E -> any (\x -> maybe True (\(c :: Cell) -> c^.L.walls^.L.right) $ Map.lookup (MapPoint x cy) (maze^.L.cells))
            [cx..(tx - 1)]

blockedByPlayer :: Maze -> PlayerPositions -> MapPoint -> MapPoint -> Dir -> Bool
blockedByPlayer maze players (MapPoint cx cy) (MapPoint tx ty) dir =
  case dir of
    N -> any (\y -> not $ null $ Map.filter (== MapPoint cx y) players) [ty..(cy - 1)]
    S -> any (\y -> not $ null $ Map.filter (== MapPoint cx y) players) [(cy + 1)..ty]
    W -> any (\x -> not $ null $ Map.filter (== MapPoint x cy) players) [tx..(cx - 1)]
    E -> any (\x -> not $ null $ Map.filter (== MapPoint x cy) players) [(cx + 1)..tx]

getDirection :: MapPoint -> MapPoint -> Maybe Dir
getDirection (MapPoint cx cy) (MapPoint tx ty) =
  if cx == tx then
    if ty > cy then
      Just S
    else if ty < cy then
      Just N
    else
      Nothing
  else if cy == ty then
    if tx > cx then
      Just E
    else if tx < cx then
      Just W
    else
      Nothing
  else
    Nothing

isEscalator :: Set Escalator -> MapPoint -> MapPoint -> Bool
isEscalator escalators mp1 mp2 = Set.member (Set.fromList [ mp1, mp2 ]) $
  Set.map (\(Escalator a b) -> Set.fromList [ a, b ]) escalators

grd :: Monoid m => Bool -> m -> m
grd True a = a
grd False _ = mempty

isValidMove :: PlayerColor -> MapPoint -> ServerGameState -> Bool
isValidMove pCol targetPos gs = maybe False (const True) $ do
  currentPos <- Map.lookup pCol (gs^.L.players)
  targetCell <- Map.lookup targetPos (gs^.L.maze^.L.cells)
  grd (targetCell^.L.special /= (Just STUnwalkable)) (pure ())
  grd (not $ any (== targetPos) (gs^.L.players)) (pure ())
  if isEscalator (gs^.L.maze^.L.escalators) currentPos targetPos ||
    (targetCell^.L.special == Just (STWarp pCol) && gs^.L.status /= WeaponsAcquired) then
    pure $ (gs & over L.players (Map.update (const $ Just targetPos) pCol))
    else do
      dir <- getDirection currentPos targetPos
      grd (not $ blockedByWall (gs^.L.maze) currentPos targetPos dir) (pure ())
      grd (not $ blockedByPlayer (gs^.L.maze) (gs^.L.players) currentPos targetPos dir) (pure ())
      pure $ (gs & over L.players (Map.update (const $ Just targetPos) pCol))

-- rotate (n * 90) deg clockwise
-- x: (y - y0) + x0
-- y: -(x - x0) + y0
rotatePoint :: Int -> MapPoint -> MapPoint
rotatePoint 0 (MapPoint x y) = MapPoint x y
rotatePoint n (MapPoint x y) = rotatePoint (n - 1) $ MapPoint (3 - y) x

translateTile :: MapPoint -> Tile -> Tile
translateTile mp t = t &
  L.cells .~ (Map.mapKeys (+ mp) (t^.L.cells)) &
  L.escalators .~ (Set.map (\(Escalator mp1 mp2) -> Escalator (mp1 + mp) (mp2 + mp)) (t^.L.escalators))

rotateCells :: Int -> Cells -> Cells
rotateCells 0 cells = cells
rotateCells n cells =
  let mp x y = MapPoint x y
      rotateDir N = E
      rotateDir E = S
      rotateDir S = W
      rotateDir W = N
      rotateSpecial (Just (STExplore col dir)) = Just $ STExplore col (rotateDir dir)
      rotateSpecial (Just (STExit col dir)) = Just $ STExit col (rotateDir dir)
      rotateSpecial s = s
  in rotateCells (n - 1) $
    foldMap
      (\x ->
        foldMap
          (\y ->
              let defaultCell = Cell { _special = Nothing, _walls = Walls False False }
                  origCell = fromMaybe defaultCell $ Map.lookup (mp x y) cells
                  eastCell = fromMaybe defaultCell $ Map.lookup (mp (x + 1) y) cells
              in Map.singleton (mp x y)
                (Cell {
                  _special = rotateSpecial (origCell^.L.special),
                  _walls = Walls (eastCell^.L.walls^.L.down) (origCell^.L.walls^.L.right)
                }))
          [0..3])
      [0..3]

-- We can't do rotateWalls n ... rotatePoint n. because the points and walls need to be rotated
-- 90 degrees at a time together.
rotateTile :: Int -> Tile -> Tile
rotateTile 0 t = t
rotateTile n t = rotateTile (n - 1) $ (t &
  L.cells .~ (rotateCells 1 $ Map.mapKeys (rotatePoint 1) (t^.L.cells)) &
  L.escalators .~ (Set.map (\(Escalator mp1 mp2) -> Escalator (rotatePoint 1 mp1) (rotatePoint 1 mp2)) (t^.L.escalators)))

addBorderWalls :: Tile -> Tile
addBorderWalls t =
  let noBorderWall (Just (STExplore _ _)) = True
      noBorderWall (Just STEntrance) = True
      noBorderWall _ = False
  in t & L.cells .~
        (Map.mapWithKey
          (\(MapPoint x y) c ->
            if noBorderWall (c^.L.special) then
              c
            else
              if x == 3 then
                if y == 3 then
                  c & L.walls .~ Walls True True
                else
                  c & over L.walls (L.right .~ True)
              else
                if y == 3 then
                  c & over L.walls (L.down .~ True)
                else
                  c)
          (t^.L.cells))

dirToInt :: Dir -> Int
dirToInt N = 0
dirToInt E = 1
dirToInt S = 2
dirToInt W = 3

getRotation :: Dir -> Dir -> Int
getRotation explore entrance = (dirToInt explore - dirToInt entrance + 2) `mod` 4

rotateAndTranslate :: MapPoint -> Dir -> Tile -> Tile
rotateAndTranslate mp dir tile  =
  let mkMp x y = MapPoint x y
      rotated = addBorderWalls $ rotateTile (getRotation dir (tile^.L.entrance^.L.side)) tile
      findEntrance f = fromMaybe 0 $ getFirst $
          foldMap
            (\i -> First $ do
                c <- Map.lookup (f i) (rotated^.L.cells)
                if c^.L.special == Just STEntrance then Just i else Nothing)
            [0..3]
      south x = mkMp x 3
      west y = mkMp 0 y
      north x = mkMp x 0
      east y = mkMp 3 y
   in case dir of
           N -> translateTile (mp + mkMp (negate $ findEntrance south) (-4)) rotated
           E -> translateTile (mp + mkMp 1 (negate $ findEntrance west)) rotated
           S -> translateTile (mp + mkMp (negate $ findEntrance north) 1) rotated
           W -> translateTile (mp + mkMp (-4) (negate $ findEntrance east)) rotated

mergeTiles :: Maze -> Tile -> MapPoint -> Dir -> Maybe Maze
mergeTiles cur newTile mp dir = do
  let mkMp x y = MapPoint x y
  let rntTile = rotateAndTranslate mp dir newTile
  let newCells = Map.union (cur^.L.cells) (rntTile^.L.cells)
  let newEscalators = Set.union (cur^.L.escalators) (rntTile^.L.escalators)
  let arrayCells = Map.toList newCells
  let xs = map (\((MapPoint x y), v) -> x) arrayCells
  let ys = map (\((MapPoint x y), v) -> y) arrayCells
  let newBorders = DirMap (minimum xs) (minimum ys) (maximum xs) (maximum ys)
  if length newCells /= length (cur^.L.cells) + 16 then Nothing else
    pure $ (cur &
      L.cells .~ newCells &
      L.borders .~ newBorders &
      L.escalators .~ newEscalators)
