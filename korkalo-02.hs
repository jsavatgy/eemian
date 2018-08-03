
import Data.IORef 
import System.IO.Unsafe
import Graphics.UI.Gtk 
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)

main = do
  initGUI
  ref <- newIORef (1.0,0.0,0.0)
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- return $ unsafeLoadPNG "500x500grey50.png"
  widgetSetSizeRequest window 600 600
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput ref window (glibToString key)
    C.liftIO $ updateCanvas1 ref canvas surf
    return ()
  canvas `on` scrollEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    d <- M.eventScrollDirection
    t <- M.eventTime
    C.liftIO $ changeRef ref d mouseX mouseY
    C.liftIO $ updateCanvas1 ref canvas surf
    C.liftIO $ putStrLn (show t) 
    C.liftIO $ putStrLn (show (mouseX,mouseY)) 
    C.liftIO $ putStrLn (show m) 
    C.liftIO $ putStrLn (show d) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 ref canvas surf)
  set window [containerChild := canvas]
  widgetShowAll window
  mainGUI

keyInput ref window key = do
  C.liftIO $ print key
  case key of
    "q" -> do
      C.liftIO $ mainQuit
    "1" -> do
      C.liftIO $ writeIORef ref (1.0,0.0,0.0)

changeRef ref d mouseX mouseY = do
  (scaleOld,oldX,oldY) <- readIORef ref
  let
    scaleD = scale1 d
    scaleNew = scaleD * scaleOld
    dx = (mouseX - oldX) * (scaleD - 1)
    dy = (mouseY - oldY) * (scaleD - 1)
    newX = oldX - dx
    newY = oldY - dy
    result =  (scaleNew,newX,newY)
  writeIORef ref result
  (putStrLn . show) result
  where
    factor = 5/4
    scale1 ScrollUp   = factor
    scale1 ScrollDown = 1/factor

updateCanvas1 ref canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 ref surf
  return True

imageSurfaceCreateFromPNG :: FilePath -> IO C.Surface
imageSurfaceCreateFromPNG file =
  C.withImageSurfaceFromPNG file $ \png -> do
    C.liftIO $ putStrLn "Load Image"
    w <- C.renderWith png $ C.imageSurfaceGetWidth png
    h <- C.renderWith png $ C.imageSurfaceGetHeight png
    surf <- C.createImageSurface C.FormatRGB24 w h
    C.renderWith surf $ do
      C.setSourceSurface png 0 0
      C.paint
    return surf

unsafeLoadPNG file = unsafePerformIO $ imageSurfaceCreateFromPNG file

paintImage1 ref surf = do
  (s,x,y) <- C.liftIO $ readIORef ref
  C.setSourceRGB 1 1 1
  C.paint
  C.translate x y
  C.scale s s
  C.liftIO $ putStrLn (show (s,x,y))
  C.liftIO $ putStrLn "Paint Image"
  C.setSourceSurface surf 0 0
  C.paint

