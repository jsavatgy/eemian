
import System.IO.Unsafe
import Graphics.UI.Gtk 
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as M

main = do
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- return $ unsafeLoadPNG "korkalo-1.png"
  widgetSetSizeRequest window 600 600
  onKeyPress window $ const (do widgetDestroy window; return True)
  canvas `on` scrollEvent $ tryEvent $ do
    (x1,y1) <- M.eventCoordinates
    m <- M.eventModifier
    s <- M.eventScrollDirection
    t <- M.eventTime
    C.liftIO $ putStrLn (show t) 
    C.liftIO $ putStrLn (show (x1,y1)) 
    C.liftIO $ putStrLn (show m) 
    C.liftIO $ putStrLn (show s) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 canvas surf)
  set window [containerChild := canvas]
  widgetShowAll window
  mainGUI

updateCanvas1 canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 surf
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

paintImage1 surf = do
  --C.scale 10 10
  C.liftIO $ putStrLn "Paint Image"
  C.setSourceSurface surf 0 0
  C.paint

