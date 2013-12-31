import Graphics.Rendering.ShivaVG.Raw
import Graphics.Rendering.ShivaVG.Raw.Unsafe
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))

w = 500 :: Int
h = 300 :: Int
sqx = 200 :: Int
sqy = 200 :: Int

-- properties of the square: translation, scale, rotation
tx = 0.0 :: Float
ty = 0.0 :: Float
sx = 1.0 :: Float
sy = 1.0 :: Float
a = 0.0 :: Float

testCreatePath :: IO Path
testCreatePath = createPath PathFormatStandard PathDatatypeF 1 0 0 0 [PathCapabilityAll]

initDrawing :: IO (Path, Paint)
initDrawing = do
  p <- testCreatePath
  redFill <- createPaint
  let (Paint ph) = redFill
  setParameterfv ph PaintColor [1,0,0,1]
      
  appendPathData p [(MoveToAbs, ((fromIntegral (w - sqx))/2, (fromIntegral (h - sqy)) / 2)),
                    (LineToRel, (fromIntegral sqx, 0)),
                    (LineToRel, (0, fromIntegral sqy)),
                    (LineToRel, (fromIntegral (-sqx), 0))]
  closePath p
  
  return (p,redFill)

display :: (Path, Paint) -> IO ()
display (p,redFill) = do
  putStrLn "display!"
  
  setfv ClearColor [0,0,0,1]
  clear 0 0 w h
  
  seti MatrixMode (fromEnum MatrixFillPaintToUser)
  
  loadIdentity
  translate tx ty
  scale sx sy
  rotate a

  seti MatrixMode (fromEnum MatrixPathUserToSurface)
  loadIdentity
  
  setPaint redFill [FillPath]
  drawPath p [FillPath]
  
  
  
  GLUT.swapBuffers
  
  

main = do
  putStrLn "Hello, OpenVG!"
  
  GLUT.initialize "OpenVG Test Program" []
  GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.WithAlphaComponent, GLUT.WithStencilBuffer, GLUT.Multisampling]
  GLUT.initialWindowPosition $= GLUT.Position 0 0
  GLUT.initialWindowSize $= GLUT.Size (fromIntegral w) (fromIntegral h)
  GLUT.createWindow "OpenVG Test"
  
  createContextSH w h
  (p,redFill) <- initDrawing
  
  GLUT.displayCallback $= (display (p,redFill))
  
  GLUT.mainLoop
  
  putStrLn "Done"