module Main where

import System.Process
import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Low

-- Spawn the COR process which will react to events written to its
-- stdin. It will also write paint commands which VID is expected to
-- execute.
spawnCore :: IO (Handle, Handle)
spawnCore = do
  (Just paintPipe, Just eventPipe, _, cor) <- createProcess
    (proc "./cor" []) { std_in = CreatePipe, std_out = CreatePipe }
  return (paintPipe, eventPipe)

data PaintCommand =
  PaintBox |
  PaintLine |
  PaintImage |
  PaintText |
  PaintFill
    deriving Show

main = do
  putStrLn "VID hello world"
  (paintIn, eventOut) <- spawnCore
  paintQueue <- newTQueueIO
  forkIO (paintWorker paintQueue)
  glfwLoop eventOut paintQueue

errorCb :: GLFW.Error -> String -> IO ()
errorCb e msg = do
  hPutStrLn stderr (unwords ["VID", show e, msg])
  exitFailure

-- Setup a GLFW window and react to input by writing to the eventPipe.
-- Independently, when you get a paint command, wait up the event listener
-- so that it can render graphics.
glfwMain :: Handle -> TQueue PaintCommand -> IO a
glfwMain eventOut paintQueue = do
  GLFW.setErrorCallback (Just errorCb)
  ok <- GLFW.init
  when (ok == False) $ do
    GLFW.pollEvents
    exitFailure
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Epichord" Nothing Nothing
  case mwin of
    Nothing  -> do
      GLFW.pollEvents
      exitFailure
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        glfwLoop eventOut paintQueue

glfwLoop :: Handle -> TQueue PaintCommand -> IO a
glfwLoop eventOut paintQueue = do
  GLFW.pollEvents
  whileM_ (not <$> atomically (isEmptyTQueue paintQueue)) $ do
    command <- readTQueue paintQueue

  GLFW.swapBuffers win

setup = do
  -- establish a VAO
  vao <- newVAO
  bindVAO vao
  -- load shader program
  vsource <- readFile "hello.vert"
  fsource <- readFile "hello.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  -- load vertex data: three 2D vertex positions
  let blob = V.fromList
        [ -0.5, -0.5
        ,    0,  0.5
        ,  0.5, -0.5 ] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  -- connect program to vertex data via the VAO
  setVertexLayout [Attrib "position" 2 GLFloat]
  return (vao, prog)

draw vao prog = do
  clearColorBuffer (0,0,0)
  bindVAO vao
  useProgram prog
  drawTriangles 3
