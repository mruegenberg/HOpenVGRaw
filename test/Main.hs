import qualified Graphics.Rendering.ShivaVG.Raw as VG
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

testCreatePath :: IO VG.Path
testCreatePath = VG.createPath VG.PathFormatStandard VG.PathDatatypeF 1 0 0 0 [VG.PathCapabilityAll]

initDrawing :: IO (VG.Path, VG.Paint)
initDrawing = do
  p <- testCreatePath
  redFill <- VG.createPaint
  VG.set (VG.paintColor redFill) (VG.SRGBA 1 0 0 1)
      
  VG.appendPathData p [(VG.MoveTo ((fromIntegral (w - sqx))/2) ((fromIntegral (h - sqy)) / 2), VG.Absolute),
                       (VG.LineTo (fromIntegral sqx) 0, VG.Relative),
                       (VG.LineTo 0 (fromIntegral sqy), VG.Relative),
                       (VG.LineTo (fromIntegral (-sqx)) 0, VG.Relative)]
  VG.closePath p
  
  return (p,redFill)

display :: (VG.Path, VG.Paint) -> IO ()
display (p,redFill) = do
  putStrLn "display!"
  
  VG.set VG.clearColor (VG.SRGBA 0 0 0 1)
  VG.clear 0 0 w h
  
  VG.set VG.matrixMode VG.MatrixFillPaintToUser
  
  VG.loadIdentity
  VG.translate tx ty
  VG.scale sx sy
  VG.rotate a
  
  VG.set VG.matrixMode VG.MatrixPathUserToSurface
  VG.loadIdentity
  
  VG.setPaint redFill [VG.FillPath]
  VG.drawPath p [VG.FillPath]
  
  
  
  GLUT.swapBuffers
  
  

main = do
  putStrLn "Hello, OpenVG!"
  
  GLUT.initialize "OpenVG Test Program" []
  GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.WithAlphaComponent, GLUT.WithStencilBuffer, GLUT.Multisampling]
  GLUT.initialWindowPosition $= GLUT.Position 0 0
  GLUT.initialWindowSize $= GLUT.Size (fromIntegral w) (fromIntegral h)
  GLUT.createWindow "OpenVG Test"
  
  VG.createContextSH w h
  (p,redFill) <- initDrawing
  
  GLUT.displayCallback $= (display (p,redFill))
  
  GLUT.mainLoop
  
  putStrLn "Done"