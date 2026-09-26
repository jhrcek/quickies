#!/usr/bin/env stack
{- stack script
--resolver lts-24.60
--package brick
--package vty
--package linear
--package containers
--optimize
--ghc-options=-threaded
-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, void)
import Data.Map.Strict qualified as M
import Graphics.Vty qualified as V
import Linear.Quaternion (Quaternion, axisAngle, rotate)
import Linear.V3 (V3 (..))

data AppState = AppState
    { rotation :: Quaternion Float
    , paused :: Bool
    }

-- A tick event to update the animation.
data Tick = Tick

initialState :: AppState
initialState =
    AppState
        { rotation = axisAngle (V3 0 0 1) 0 -- Identity quaternion
        , paused = False
        }

rotationStep :: Float
rotationStep = 0.1

-- Rotation applied on every tick while running: small rotations around X and Y axes
autoRotation :: Quaternion Float
autoRotation =
    axisAngle (V3 1 0 0) (rotationStep * 0.3)
        * axisAngle (V3 0 1 0) (rotationStep * 0.5)

-- Manual rotation controls (only active when paused)
keyRotation :: Char -> Maybe (Quaternion Float)
keyRotation k = case k of
    'q' -> Just $ axisAngle (V3 0 0 1) rotationStep
    'e' -> Just $ axisAngle (V3 0 0 1) (-rotationStep)
    'w' -> Just $ axisAngle (V3 1 0 0) rotationStep
    's' -> Just $ axisAngle (V3 1 0 0) (-rotationStep)
    'a' -> Just $ axisAngle (V3 0 1 0) rotationStep
    'd' -> Just $ axisAngle (V3 0 1 0) (-rotationStep)
    _ -> Nothing

app :: App AppState Tick ()
app =
    App
        { appDraw = \s -> [drawStatusBar s <=> drawCube s]
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = pure ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

appEvent :: BrickEvent n Tick -> EventM n AppState ()
appEvent e = do
    isPaused <- gets paused
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey (V.KChar ' ') []) -> modify $ \s -> s{paused = not isPaused}
        AppEvent Tick -> unless isPaused $ applyRotation autoRotation
        VtyEvent (V.EvKey (V.KChar k) [])
            | isPaused, Just r <- keyRotation k -> applyRotation r
        _ -> pure ()

applyRotation :: Quaternion Float -> EventM n AppState ()
applyRotation r = modify $ \s@AppState{rotation} -> s{rotation = rotation * r}

drawStatusBar :: AppState -> Widget n
drawStatusBar AppState{paused} =
    let pauseLabel = if paused then "Paused " else "Running"
        controls =
            if paused
                then "[SPACE] Resume | [QWEASD] Rotate | [ESC] Quit"
                else "[SPACE] Pause  |                 | [ESC] Quit"
     in hCenter $
            borderWithLabel (str " 3D Cube Animation ") $
                hBox
                    [ padLeftRight 1 $ str $ "Status: " ++ pauseLabel
                    , padLeftRight 1 $ str controls
                    ]

-- Fill all the space left over by the status bar with the cube
drawCube :: AppState -> Widget n
drawCube AppState{rotation} = Widget Greedy Greedy $ do
    ctx <- getContext
    render $ str $ renderCube (availWidth ctx) (availHeight ctx) rotation

cubeVertices :: [V3 Float]
cubeVertices = [V3 x y z | x <- [-1, 1], y <- [-1, 1], z <- [-1, 1]]

cubeEdges :: [(V3 Float, V3 Float)]
cubeEdges =
    [ (v1, v2)
    | v1 <- cubeVertices
    , v2 <- cubeVertices
    , v1 < v2 -- Each edge only once
    , sum (abs (v1 - v2)) == 2 -- Connect vertices that differ in exactly one coordinate
    ]

-- Simplified Bresenham's Line Algorithm
bresenhamLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bresenhamLine (x0, y0) (x1, y1) = go x0 y0 (dx - dy)
  where
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    sx = signum (x1 - x0)
    sy = signum (y1 - y0)
    go x y err
        | x == x1 && y == y1 = [(x, y)]
        | otherwise =
            let e2 = 2 * err
                (x', errX) = if e2 > -dy then (x + sx, err - dy) else (x, err)
                (y', errY) = if e2 < dx then (y + sy, errX + dx) else (y, errX)
             in (x, y) : go x' y' errY

-- Choose a character based on depth (lower z = closer to viewer = bigger dot)
chooseChar :: Float -> Float -> Float -> Char
chooseChar minZ maxZ z
    | z <= minZ + range / 3 = '⏺'
    | z >= maxZ - range / 3 = '•'
    | otherwise = '●'
  where
    range = maxZ - minZ

renderCube :: Int -> Int -> Quaternion Float -> String
renderCube width height rot =
    unlines
        [ [M.findWithDefault ' ' (x, y) grid | x <- [0 .. width - 1]]
        | y <- [0 .. height - 1]
        ]
  where
    -- Vertices come last so they overwrite edge points (M.fromList keeps the last value)
    grid = M.fromList $ concatMap edgePoints cubeEdges ++ map vertexPoint cubeVertices

    edgePoints (v1, v2) =
        let (p1, z1) = projectVertex v1
            (p2, z2) = projectVertex v2
            points = bresenhamLine p1 p2
            lastIdx = max 1 (length points - 1)
         in [ (p, depthChar (z1 + t * (z2 - z1)))
            | (i, p) <- zip [0 :: Int ..] points
            , let t = fromIntegral i / fromIntegral lastIdx
            ]

    vertexPoint v =
        let (p, z) = projectVertex v
         in (p, depthChar z)

    -- Rotate vertex and project it to 2D screen coordinates, keeping its depth
    projectVertex v =
        let V3 x y z = rotate rot v
            factor = scale / (z + viewerDistance)
         in ((width `div` 2 + round (x * factor), height `div` 2 - round (y * factor)), z)

    viewerDistance = 3
    -- Empirical factor that makes the cube fill ~95% of the smaller dimension
    scale = 0.95 * fromIntegral (min width height) / 1.4

    depthChar = chooseChar (minimum zs) (maximum zs)
    zs = [z | V3 _ _ z <- map (rotate rot) cubeVertices]

main :: IO ()
main = do
    chan <- newBChan 10
    -- Send a Tick every 50ms; pausing is handled in the event handler.
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 50_000
    void $ customMainWithDefaultVty (Just chan) app initialState
