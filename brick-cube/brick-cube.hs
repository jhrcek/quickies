#!/usr/bin/env stack
{- stack script
--resolver lts-22.43
--package brick
--package vty
--package vty-crossplatform
--package linear
--package containers
--optimize
--ghc-options=-threaded
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Map.Strict qualified as M
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform as V
import Linear.Matrix (M33, (!*))
import Linear.V3 (V3 (..))

data AppState = AppState
    { angle :: Float
    , termWidth :: Int
    , termHeight :: Int
    , cubeScale :: Float
    , paused :: Bool
    }

-- A tick event to update the animation.
data Tick = Tick

initialState :: Int -> Int -> AppState
initialState width height =
    setDimensions width height $
        AppState
            { angle = 0
            , termWidth = 0
            , termHeight = 0
            , cubeScale = 0
            , paused = False
            }

-- Calculate the optimal cube scale (95% of smaller dimension)
calculateCubeScale :: Int -> Int -> Float
calculateCubeScale width height =
    let smallerDimension = min width height
     in -- Use 95% of the smaller dimension, and divide by 1.4 since
        -- the cube vertices range from -1 to 1 (total size is 1.4 units per dimension)
        0.95 * fromIntegral smallerDimension / 1.4

app :: App AppState Tick ()
app =
    App
        { appDraw = \s -> [drawUI s]
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = pure ()
        , appAttrMap = const theMap
        }

theMap :: AttrMap
theMap = attrMap V.defAttr []

appEvent :: BrickEvent n Tick -> EventM n AppState ()
appEvent (AppEvent Tick) = modify $ \s -> if paused s then s else s{angle = angle s + 0.1}
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = togglePause
appEvent (VtyEvent (V.EvResize width height)) = modify (setDimensions width height)
appEvent _ = pure ()

togglePause :: EventM n AppState ()
togglePause = modify $ \s -> s{paused = not (paused s)}

setDimensions :: Int -> Int -> AppState -> AppState
setDimensions width height s =
    s
        { termWidth = width
        , termHeight = height
        , cubeScale = calculateCubeScale width height
        }

drawUI :: AppState -> Widget n
drawUI s =
    let statusBar = drawStatusBar s
        cubeWidget = center $ str (renderCube s)
     in statusBar <=> cubeWidget

drawStatusBar :: AppState -> Widget n
drawStatusBar AppState{paused} =
    let pauseLabel =
            if paused
                then "Paused "
                else "Running"
        controls = " [SPACE] Pause/Resume | [Q/ESC] Quit "
     in hCenter $
            borderWithLabel (str " 3D Cube Animation ") $
                hBox
                    [ padLeftRight 1 $ str $ "Status: " ++ pauseLabel
                    , padLeftRight 3 $ str controls
                    ]

cubeVertices :: [V3 Float]
cubeVertices = [V3 x y z | x <- [-1, 1], y <- [-1, 1], z <- [-1, 1]]

cubeEdges :: [(V3 Float, V3 Float)]
cubeEdges =
    [ (v1, v2)
    | v1 <- cubeVertices
    , v2 <- cubeVertices
    , sum (abs (v1 - v2)) == 2 -- Connect vertices that differ in exactly one coordinate
    ]

-- Create rotation matrix around X axis
rotationMatrixX :: Float -> M33 Float
rotationMatrixX theta =
    V3
        (V3 1 0 0)
        (V3 0 (cos theta) (-(sin theta)))
        (V3 0 (sin theta) (cos theta))

-- Create rotation matrix around Y axis
rotationMatrixY :: Float -> M33 Float
rotationMatrixY theta =
    V3
        (V3 (cos theta) 0 (sin theta))
        (V3 0 1 0)
        (V3 (-(sin theta)) 0 (cos theta))

-- Apply rotations to a vertex
rotate :: Float -> V3 Float -> V3 Float
rotate theta pt = rotationMatrixY theta !* (rotationMatrixX theta !* pt)

-- Project 3D point to 2D screen coordinates
project2D :: AppState -> V3 Float -> (Int, Int)
project2D s (V3 x y z) =
    let d = 3.0 -- Distance from the viewer
        scale = cubeScale s -- Use dynamic scale from AppState
        factor = scale / (z + d)
        x' = x * factor
        y' = y * factor
        -- Center the cube in the terminal
        centerX = termWidth s `div` 2
        centerY = termHeight s `div` 2
     in (centerX + round x', centerY - round y')

-- Simplified Bresenham's Line Algorithm
bresenhamLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bresenhamLine (x0, y0) (x1, y1) =
    let dx = abs (x1 - x0)
        dy = abs (y1 - y0)
        sx = signum (x1 - x0)
        sy = signum (y1 - y0)

        plotLine x y err =
            (x, y)
                : if x == x1 && y == y1
                    then []
                    else
                        let (x', y', err') = step x y err
                         in plotLine x' y' err'

        step x y err =
            let e2 = 2 * err
                (nx, ne) = if e2 > (-dy) then (x + sx, err - dy) else (x, err)
                (ny, ne') = if e2 < dx then (y + sy, ne + dx) else (y, ne)
             in (nx, ny, ne')
     in plotLine x0 y0 (dx - dy)

-- Choose a character based on depth
chooseChar :: Float -> Float -> Float -> Char
chooseChar z minZ maxZ
    | z <= minZ + range / 3 = '⏺'
    | z >= maxZ - range / 3 = '•'
    | otherwise = '●'
  where
    range = maxZ - minZ

renderCube :: AppState -> String
renderCube s@AppState{angle, termWidth, termHeight} =
    let
        -- Use a Map to store characters
        initialGrid = M.empty

        -- Add edges to grid
        gridWithEdges = foldl addEdge initialGrid cubeEdges

        -- Add vertices to grid
        finalGrid = foldl addVertex gridWithEdges rotatedVerts

        -- Convert grid to string
        renderGrid =
            unlines
                [ [ M.findWithDefault ' ' (x, y) finalGrid
                  | x <- [0 .. termWidth - 1]
                  ]
                | y <- [0 .. termHeight - 1]
                ]
     in
        renderGrid
  where
    -- Add an edge to the grid
    addEdge grid (v1, v2) =
        let rv1 = rotate angle v1
            rv2 = rotate angle v2
            p1 = project2D s rv1
            p2 = project2D s rv2
            V3 _ _ z1 = rv1
            V3 _ _ z2 = rv2
            linePoints = bresenhamLine p1 p2

            -- For each point on the line, add a character to the grid
            addLinePoint g ((x, y), t) =
                let zInterp = z1 + t * (z2 - z1)
                    ch = chooseChar zInterp globalMinZ globalMaxZ
                 in M.insert (x, y) ch g

            -- Calculate interpolation factors for each point
            n = length linePoints
            interpolated = zip linePoints [if n > 1 then fromIntegral i / fromIntegral (n - 1) else 0 | i <- [0 .. n - 1]]
         in foldl addLinePoint grid interpolated

    -- Add a vertex to the grid
    addVertex grid rv =
        let p = project2D s rv
            V3 _ _ z = rv
            ch = chooseChar z globalMinZ globalMaxZ
         in M.insert p ch grid

    rotatedVerts = map (rotate angle) cubeVertices
    globalMinZ = minimum $ map (\(V3 _ _ z) -> z) rotatedVerts
    globalMaxZ = maximum $ map (\(V3 _ _ z) -> z) rotatedVerts

main :: IO ()
main = do
    chan <- newBChan 10
    -- Fork a thread that sends a Tick event every 50ms when not paused.
    -- We'll manage pausing in the event handler instead of here.
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 50_000 -- 50ms
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    (width, height) <- V.displayBounds $ V.outputIface initialVty
    let initState = initialState width height
    void $ customMain initialVty buildVty (Just chan) app initState
