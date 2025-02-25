#!/usr/bin/env stack
{- stack script
--resolver lts-22.43
--package brick
--package vty
--package vty-crossplatform
--optimize
--ghc-options=-threaded
-}

{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform as V

--------------------------------------------------------------------------------
-- Application State and Events

-- Updated AppState to include terminal dimensions and scale
data AppState = AppState
    { angle :: Float
    , termWidth :: Int
    , termHeight :: Int
    , cubeScale :: Float
    }

-- A tick event to update the animation.
data Tick = Tick

-- Initialize with placeholder values that will be updated in appStartEvent
initialState :: AppState
initialState =
    AppState
        { angle = 0
        , termWidth = 80 -- Will be updated on start
        , termHeight = 40 -- Will be updated on start
        , cubeScale = 20.0 -- Will be updated on start
        }

-- Calculate the optimal cube scale (95% of smaller dimension)
calculateCubeScale :: Int -> Int -> Float
calculateCubeScale width height =
    let smallerDimension = min width height
        -- Use 95% of the smaller dimension, and divide by 1.4 since
        -- the cube vertices range from -1 to 1 (total size is 1.4 units per dimension)
        scale = 0.95 * fromIntegral smallerDimension / 1.4
     in scale

--------------------------------------------------------------------------------
-- Brick App Definition

app :: App AppState Tick ()
app =
    App
        { appDraw = \s -> [drawUI s]
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = appStart
        , appAttrMap = const theMap
        }

-- Add startup event handler to get terminal dimensions
appStart :: EventM n AppState ()
appStart = do
    vty <- getVtyHandle
    (width, height) <- liftIO $ V.displayBounds $ V.outputIface vty
    let scale = calculateCubeScale width height
    modify $ \s ->
        s
            { termWidth = width
            , termHeight = height
            , cubeScale = scale
            }

theMap :: AttrMap
theMap = attrMap V.defAttr []

appEvent :: BrickEvent n Tick -> EventM n AppState ()
appEvent (AppEvent Tick) =
    -- Update the angle for a smooth rotation.
    modify $ \s -> s{angle = angle s + 0.1}
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
-- Update scale on terminal resize
appEvent (VtyEvent (V.EvResize width height)) = do
    let scale = calculateCubeScale width height
    modify $ \s ->
        s
            { termWidth = width
            , termHeight = height
            , cubeScale = scale
            }
appEvent _ = pure ()

drawUI :: AppState -> Widget n
drawUI s = center $ str (renderCube s)

--------------------------------------------------------------------------------
-- Cube Definition, Rotation, and Projection

-- Define the eight vertices of a cube.
cubeVertices :: [(Float, Float, Float)]
cubeVertices = [(x, y, z) | x <- [-1, 1], y <- [-1, 1], z <- [-1, 1]]

-- An edge connects two vertices that differ in exactly one coordinate.
cubeEdges :: [((Float, Float, Float), (Float, Float, Float))]
cubeEdges =
    [ (v1, v2)
    | v1 <- cubeVertices
    , v2 <- cubeVertices
    , isEdge v1 v2
    ]
  where
    isEdge (x1, y1, z1) (x2, y2, z2) =
        let dx = abs (x1 - x2)
            dy = abs (y1 - y2)
            dz = abs (z1 - z2)
         in (dx + dy + dz) == 2

-- Rotate a 3D point around the X axis.
rotateX :: Float -> (Float, Float, Float) -> (Float, Float, Float)
rotateX theta (x, y, z) =
    ( x
    , y * cos theta - z * sin theta
    , y * sin theta + z * cos theta
    )

-- Rotate a 3D point around the Y axis.
rotateY :: Float -> (Float, Float, Float) -> (Float, Float, Float)
rotateY theta (x, y, z) =
    ( x * cos theta + z * sin theta
    , y
    , -(x * sin theta) + z * cos theta
    )

-- Apply two rotations (around X then Y) to a vertex.
rotate :: Float -> (Float, Float, Float) -> (Float, Float, Float)
rotate theta pt = rotateY theta (rotateX theta pt)

-- Updated project to use dynamic scaling from AppState
project :: AppState -> (Float, Float, Float) -> (Int, Int)
project s (x, y, z) =
    let d = 3.0 -- Distance from the viewer
        scale = cubeScale s -- Use dynamic scale from AppState
        factor = scale / (z + d)
        x' = x * factor
        y' = y * factor
        -- Center the cube in the terminal
        centerX = termWidth s `div` 2
        centerY = termHeight s `div` 2
     in (centerX + round x', centerY - round y')

--------------------------------------------------------------------------------
-- Bresenham's Line Algorithm
-- Returns a list of points between the start and end points.

bresenhamLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bresenhamLine (x0, y0) (x1, y1) = go x0 y0 err0 []
  where
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    err0 = dx - dy
    go x y err acc
        | x == x1 && y == y1 = acc ++ [(x, y)]
        | otherwise =
            let e2 = 2 * err
                (x', err') =
                    if e2 > (-dy)
                        then (x + sx, err - dy)
                        else (x, err)
                (y', err'') =
                    if e2 < dx
                        then (y + sy, err' + dx)
                        else (y, err')
             in go x' y' err'' (acc ++ [(x, y)])

--------------------------------------------------------------------------------
-- Add helper function to choose a character based on depth.
chooseChar :: Float -> Float -> Float -> Char
chooseChar z minZ maxZ =
    let range = maxZ - minZ
     in if
            | z <= minZ + range / 3 -> '⏺'
            | z >= maxZ - range / 3 -> '•'
            | otherwise -> '●'

--------------------------------------------------------------------------------
-- Rendering the Cube to an ASCII Grid

-- Updated renderCube to take AppState instead of just angle
renderCube :: AppState -> String
renderCube s =
    let width = termWidth s
        height = termHeight s
        theta = angle s
        emptyGrid = replicate height (replicate width ' ')
        -- Compute rotated vertices for depth shading
        rotatedVerts = map (rotate theta) cubeVertices
        globalMinZ = minimum (map (\(_, _, z) -> z) rotatedVerts)
        globalMaxZ = maximum (map (\(_, _, z) -> z) rotatedVerts)
        -- Draw each edge with depth-based character shading
        gridWithEdges = foldl drawEdge emptyGrid cubeEdges
          where
            drawEdge grid (v1, v2) =
                let p1 = project s (rotate theta v1)
                    p2 = project s (rotate theta v2)
                    -- Get rotated endpoints for z values
                    (_, _, z1) = rotate theta v1
                    (_, _, z2) = rotate theta v2
                    linePoints = bresenhamLine p1 p2
                    n = length linePoints
                    -- For each point interpolate the depth and choose a character
                    grid' =
                        foldl
                            ( \g (idx, (x, y)) ->
                                let t = if n > 1 then fromIntegral idx / fromIntegral (n - 1) else 0
                                    zInterp = z1 + t * (z2 - z1)
                                    ch = chooseChar zInterp globalMinZ globalMaxZ
                                 in setChar g x y ch
                            )
                            grid
                            (zip [0 :: Int ..] linePoints)
                 in grid'
        -- Mark vertices with depth-based shading
        gridWithVertices =
            foldl
                ( \g (v, p) ->
                    let (_, _, z) = rotate theta v
                        ch = chooseChar z globalMinZ globalMaxZ
                     in uncurry (setChar g) p ch
                )
                gridWithEdges
                (zip cubeVertices (map (project s . rotate theta) cubeVertices))
     in unlines gridWithVertices

-- Set a character in the grid at the given (x, y) position.
setChar :: [String] -> Int -> Int -> Char -> [String]
setChar grid x y char
    | y < 0 || y >= length grid = grid
    | x < 0 || x >= length (grid !! y) = grid
    | otherwise =
        let row = grid !! y
            newRow = take x row ++ [char] ++ drop (x + 1) row
         in take y grid ++ [newRow] ++ drop (y + 1) grid

--------------------------------------------------------------------------------
-- Main: Run the Brick Application with a Timer Thread

main :: IO ()
main = do
    chan <- newBChan 10
    -- Fork a thread that sends a Tick event every 50ms.
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 50000 -- 50ms
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialState
