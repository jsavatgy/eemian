module Eemian 
(
  tikzPicture,
  Point(..), Shape(..), Layer(..),
  RGB, Color(..), colorRGB,
  Direction(..),
  namedSymbPos, mkPoint, dist,
  Rect(..), fromRect, mkVector, Vector(..),
  intToDouble, intersection, toPoint, 
  AngleType(..), add, sub, degrees, gons, radians, 
  halfpi, twopi, cos1, sin1, angleBt, axisX, axisY,
  HandedPoint(..), pt, angleFromNorthLH, dot3, toRH,  
  pointFromPolarLH, move, ptToRH, toTuple,
  printAngle, shorten, showDoublesWith, showDecimals,
  linesFromEdgeList, extractPoints, explode, 
  annotate, annotRadius, annotRadiusE, AnnotDir(..),
  toPointsPP, toPointsPg, tangents, GrabType(..),
  explodeC, mirror, AxisType(..), mkPolyLines,
  originalBoundingBox
)
where

import Data.List
import Data.Fixed
import qualified Data.Map as Map
import Data.List (sortBy,nub)
import Data.List.Utils (replace)
import Data.List.Split (splitOn,chunksOf)
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import Data.Function (on)
import Text.Printf

data Point = Point Double Double
  deriving Show

data Shape = Circle Double Point
  | Arc Double Point Angle Angle
  | Node Point
  | Line Point Point
  | Polygon [Point]
  | Grid Double Point Point
  | PhantomS Point Point
  | PolyLine [Point]
  | Named String Shape
  | NamedSymbPos String String Direction Shape
  | ArcLine [ArcLineElem]
  | Closed Shape
  | Filled Shape
  | Annotation String [Distantia]
  | AnnotationR String Double Double Point Angle
  | AnnotationRE String Double Double Double Point Angle
  | Text Point String
  deriving Show

type Name = String
type Attr = String

data Layer = Layer Name Color [Shape] Attr
  | Phantom Point Point
  deriving (Show)

data RGB = RGB Double Double Double
  deriving Show

data Color = Black | White
  | Green | DarkGreen | Blue | DarkBlue | Violet 
  | DarkViolet | Red | Orange | Yellow | Pink | RedViolet 
  | Brown | DarkBrown | Grey20 | Grey40 | Grey60 | Grey80
  deriving (Eq,Ord,Enum,Read,Show)

colorRGB Black  = RGB 0.00 0.00 0.00
colorRGB White  = RGB 1.00 1.00 1.00 
colorRGB Red    = RGB 0.88 0.29 0.22
colorRGB Orange = RGB 0.98 0.63 0.15
colorRGB Yellow = RGB 0.97 0.85 0.39
colorRGB Green  = RGB 0.38 0.74 0.43
colorRGB Blue   = RGB 0.33 0.67 0.82
colorRGB Violet = RGB 0.69 0.61 0.85
colorRGB Pink   = RGB 0.96 0.57 0.70
colorRGB Brown  = RGB 0.81 0.65 0.53
colorRGB DarkGreen  = RGB 0.00 0.66 0.52
colorRGB DarkBlue   = RGB 0.17 0.51 0.79
colorRGB DarkViolet = RGB 0.48 0.39 0.75
colorRGB DarkBrown  = RGB 0.67 0.45 0.27
colorRGB RedViolet  = RGB 0.79 0.37 0.51
colorRGB Grey20 = grey 0.20
colorRGB Grey40 = grey 0.40
colorRGB Grey60 = grey 0.60
colorRGB Grey80 = grey 0.80
grey n = RGB n n n

colorlist = [Green .. Grey80]
tikzColorList = [tikzColorDef c | c <- colorlist]
tikzColorDef name = 
  "\\definecolor{" ++ show name ++ "}{rgb}{" ++ 
  show r ++ "," ++ show g ++ "," ++ show b ++"}\n"
  where 
    RGB r g b = colorRGB name
layerColorList layers = map colorStr nubcs
  where
    nubcs = nub cs
    cs = [c | Layer n c s a <- layers]
colorStr color = tikzColorDef color

-- data Layer = Layer Name Color [Shape] Attr

tikzPicture layers = 
  prePict ++ 
  concat (layerColorList layers) ++
  concat (concat (tikzLayerList layers)) ++
  postPict

named prefix xs = [Named (name n) x | (n,x) <- zip [1..] xs] 
  where
    name n = "$" ++ prefix ++ "_{" ++ show n ++ "}$"

namedSymbPos prefix symbol pos xs = 
  [NamedSymbPos (name n) symbol pos x | (n,x) <- zip [1..] xs] 
  where
    name n = "$" ++ prefix ++ "_{" ++ show n ++ "}$"

baseName fullname = filter isAlphaNum fullname

data HandedPoint a = RH (a,a) | LH (a,a) 
  deriving Show

pointFromPolarLH t s = LH (x,y)
  where
    x = s * cos1 t
    y = s * sin1 t

toRH (LH (x,y))  = RH (y,x)
ptToRH (Point x y) = RH (y,x)

dot3 :: HandedPoint Double -> String
dot3 (RH (x,y)) = "RH (" ++ 
  printf "%.3f" x ++ "," ++
  printf "%.3f" y ++ ")"
dot3 (LH (y,x)) = "LH (" ++ 
  printf "%.3f" y ++ "," ++
  printf "%.3f" x ++ ")"

northFrom (Point x y) = Point x (y+1)

pt (RH (x,y)) = Point x y
pt (LH (x,y)) = Point y x
move (RH (x1,y1)) (RH (x2,y2)) = RH (x1+x2,y1+y2) 
move (LH (x1,y1)) (LH (x2,y2)) = LH (x1+x2,y1+y2) 

-- opposite order means opposite direction
angleFromNorthLH p1 p2 = angleBt vecApLp vecNorth 
  where
    point1 = pt p1
    point2 = pt p2
    vecApLp = mkVector point1 point2
    vecNorth = mkVector point1 (northFrom point1)

data Direction = N | S | E | W | NE | NW | SE | SW 
  deriving Show

tikzDirection N = "above"
tikzDirection S = "below"
tikzDirection E = "right"
tikzDirection W = "left"
tikzDirection NE = "above right"
tikzDirection NW = "above left"
tikzDirection SE = "below right"
tikzDirection SW = "below left"

combineColor [] color = "[" ++ show color ++ "]"
combineColor attr color = [head attr] ++ show color ++ "," ++ tail attr

tikzLayerList layers = 
  [ layerStrs (shapesOnPage s layers) attr color name
  | Layer name color s attr <- layers] 
layerStrs shapes attr color name = 
  [line1] ++ tikzStrs shapes name ++ [line2]
  where
    line1 = "\n\\begin{scope}" ++ 
      combineColor attr color ++ "\n"
    line2 = "\\end{scope}"
tikzStrs shapes layer = [
  drawTikz s ("l" ++ layer ++ "-p" ++ show n) 
  | (s,n) <- zip shapes [1..] ] 

toPointsPP (PolyPoint pts) = pts
toPointsPg (Polygon pts) = pts
explode (Polygon pts) = [ Line pa pb 
  | (pa,pb) <- zip pts (tail pts ++ cycle pts) ]
explode (PolyLine pts) = [ Line pa pb 
  | (pa,pb) <- zip pts (tail pts) ]
extractPoints (PolyLine pts) = [(x,y) | Point x y <- pts]
showdeg (DEG d) = show d

data AngleType a = RAD a | DEG a | GON a 
  deriving (Eq,Ord,Show)

type Angle = AngleType Double

degrees (RAD r) = DEG ((r * 180 / pi) `mod'` 360)
degrees (GON g) = DEG ((g * 180 / 200) `mod'` 360)
gons (RAD r) = GON ((r * 200 / pi) `mod'` 400)
radians (GON g) = RAD (g * pi / 200)
radians (DEG d) = RAD (d * pi / 180)
radians (RAD r) = RAD r
degreesNonMod (RAD r) = DEG (r * 180 / pi)
degreesNonMod (GON g) = DEG (g * 180 / 200)
degreesNonMod (DEG d) = DEG d

printAngle fmt (RAD r) = "RAD " ++ printf fmt (r::Double)
printAngle fmt (GON g) = "GON " ++ printf fmt (g::Double)
printAngle fmt (DEG d) = "DEG " ++ printf fmt (d::Double)

halfpi = pi/2
twopi  = 2*pi

cos1 (RAD r) = cos r
cos1 r = cos1 (radians (r))

sin1 (RAD r) = sin r
sin1 r = sin1 (radians (r))

add (DEG a) (DEG b) = DEG (a + b)
add (RAD a) (RAD b) = RAD (a + b)
add (GON a) (GON b) = GON (a + b)
add (DEG a) (RAD b) = radians (DEG a) `add` (RAD b)
add (RAD a) (DEG b) = (RAD a) `add` radians (DEG b)
add (GON a) (RAD b) = radians (GON a) `add` (RAD b)
add (RAD a) (GON b) = (RAD a) `add` radians (GON b)

sub (DEG a) (DEG b) = DEG (a - b)
sub (RAD a) (RAD b) = RAD (a - b)
sub (GON a) (GON b) = GON (a - b)
sub (DEG a) (RAD b) = radians (DEG a) `sub` (RAD b)
sub (RAD a) (DEG b) = (RAD a) `sub` radians (DEG b)
sub (GON a) (RAD b) = radians (GON a) `sub` (RAD b)
sub (RAD a) (GON b) = (RAD a) `sub` radians (GON b)

showDecimals :: Int -> Double -> String
showDecimals n x = printf ("%." ++ show n ++ "f") x

showDoublesWith :: Int -> [Double] -> String
showDoublesWith n xs = "[" ++ list1 ++ "]"
  where
    list1 = intercalate "," (map (showDecimals n) xs)

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

rotate t (x,y) = (xPrim,yPrim)
  where -- counterclockwise
    xPrim = x * cos1 t - y * sin1 t
    yPrim = x * sin1 t + y * cos1 t

dist (Point x0 y0) (Point x1 y1) = 
  sqrt ((sqr dx) + (sqr dy))
  where
    sqr x = x * x
    dx = x1 - x0
    dy = y1 - y0

toTuple (Point x y) = (x,y)
mkPoint (x,y) = Point x y

data Vector = Vector Double Double
  deriving Show

axisX = Vector 1.0 0.0
axisY = Vector 0.0 1.0

mkVector (Point x0 y0) (Point x1 y1) =
  Vector (x1 - x0) (y1 - y0)

angleBt (Vector x1 y1) (Vector x2 y2) = RAD t
  where
    t = atan2 (x1*y2 - y1*x2) (x1*x2 + y1*y2)

intersection (Point x1 y1) (Point x2 y2) 
  (Point x3 y3) (Point x4 y4) 
  | parallel = Nothing
  | otherwise = Just (Point x y)
  where   
    x  = nx / d
    y  = ny / d
    nx = (x1*y2-y1*x2) * (x3-x4) - (x1-x2) * (x3*y4-y3*x4)
    ny = (x1*y2-y1*x2) * (y3-y4) - (y1-y2) * (x3*y4-y3*x4)
    d  = (x1-x2) * (y3-y4) - (y1-y2) * (x3-x4)
    parallel = (d == 0)
-- en.wikipedia.org/wiki/Line-line_intersection

circleLineIntersections circle (Point x1 y1) (Point x2 y2) = pts2
  where
    Circle r (Point x0 y0) = circle
    pts2 = [Point (x1+x0) (y1+y0) | Point x1 y1 <- pts1]
    pts1 = circleLineIntersections1 r 
      (Point (x1-x0) (y1-y0)) 
      (Point (x2-x0) (y2-y0))

circleLineIntersections1 r (Point x1 y1) (Point x2 y2)
  | discr < 0  = []
  | discr == 0 = [Point x3 y3]
  | discr > 0  = [Point x3 y3, Point x4 y4]
  where
    sqr x = x * x
    dx = x2 - x1
    dy = y2 - y1
    dr = sqrt ((sqr dx) + (sqr dy))
    det = x1 * y2 - x2 * y1
    sign x 
      | x < 0  = (-1)
      | otherwise = 1
    discr = sqr r * sqr dr - sqr det
    x3 = (det * dy + sign dy * dx * sqrt discr) / (sqr dr)
    y3 = ((-det) * dx + abs dy * sqrt discr) / (sqr dr)
    x4 = (det * dy - sign dy * dx * sqrt discr) / (sqr dr)
    y4 = ((-det) * dx - abs dy * sqrt discr) / (sqr dr)
-- mathworld.wolfram.com/Circle-LineIntersection.html 

opposite (Vector dx dy) = Vector (-dx) (-dy)

vectorFromAngle r a =
  Vector (r * cos a) (r * sin a)

toPoint (Point x0 y0) (Vector dx dy) =
  Point (x0 + dx) (y0 + dy)

data AxisType = X | Y 

mirror1 X (x1,y1) = (-x1,y1)
mirror1 Y (x1,y1) = (x1,-y1)
mirror ax (PolyLine pts) = 
  PolyLine [mkPoint (mirror1 ax (toTuple p)) | p <- pts]
mirror Y (Arc r p start end) = 
  Arc r (mkPoint (mirror1 Y (toTuple p))) 
    (RAD twopi `sub` radians end) 
    (RAD twopi `sub` radians start) 
mirror X (Arc r p start end) = 
  Arc r (mkPoint (mirror1 X (toTuple p))) 
    (RAD pi `sub` radians end) 
    (RAD pi `sub` radians start) 

---------------------------------------------------------
-- Bounding boxes:
---------------------------------------------------------
{-
data Shape = Circle Double Point
  | Arc Double Point Angle Angle
  | Node Point
  | Line Point Point
  | Polygon [Point]
  | PolyLine [Point]
  | Named String Shape
  | NamedSymbPos String String Direction Shape
  | ArcLine [ArcLineElem]
  | Closed Shape
  | Filled Shape
  | Annotation String [Distantia]
  | AnnotationR String Double Double Point Angle
  | AnnotationRE String Double Double Double Point Angle
  | Text Point String
data Layer = Layer Name Color [Shape] Attr
-}

---------------------------------------------------------

bbMargin = 0.5

originalBoundingBox layers = 
  boundingBoxes shapes 
  where
    shapes = concat ([s | Layer n c s a <- layers] ++
      [[PhantomS p1 p2] | Phantom p1 p2  <- layers])

boundingBoxes shapes = (
   Point (minimum xs) (minimum ys),
   Point (maximum xs) (maximum ys) )
  where
    pts = concat [boundingBox s | s <- shapes]
    xs = [x | Point x y <- pts]
    ys = [y | Point x y <- pts]

boundingBox (NamedSymbPos s1 s2 d shape) = 
  boundingBox shape

boundingBox (Named str x) =
  boundingBox x

boundingBox (Filled x) = 
  boundingBox x

boundingBox (Text pt name) = [pt]

boundingBox (Node (Point x y)) = [
  Point (x-bbMargin) (y-bbMargin),
  Point (x+bbMargin) (y+bbMargin) ]

boundingBox (Circle r (Point x y)) = [
  Point (x-r) (y-r),
  Point (x+r) (y+r) ]

boundingBox (Arc r (Point x y) start end) = [
  Point (x-r) (y-r),
  Point (x+r) (y+r) ]

boundingBox (Line p1 p2) = [p1,p2]

boundingBox (PhantomS p1 p2) = [p1,p2]

boundingBox (Grid d p1 p2) = [p1,p2]

boundingBox (Polygon ps) = ps

boundingBox (PolyLine ps) = ps

boundingBox (Annotation fmt dds) = 
  concat [pts | Distantia d (PolyPoint pts) <- dds]

boundingBox (AnnotationR fmt r1 r2 p angle) = [p]

boundingBox (AnnotationRE fmt extra r1 r2 p angle) =
  [p, towards angle p extra]


boundingBox x = error (
  "ERROR: Boundingbox not implemented for " ++ show x )
---------------------------------------------------------
--  On Page Coordinates
---------------------------------------------------------
shapesOnPage shapes layers = 
  map onPage shapes
  where
    onPage (NamedSymbPos str1 str2 dir (Node pt)) =
      NamedSymbPos str1 str2 dir (Node (px pt))
    onPage (Named str x) = 
      Named str (onPage x)
    onPage (Node pt) = 
      Node (px pt)
    onPage (Filled x) = 
      Filled (onPage x)
    onPage (Circle r pt) = 
      Circle (dx r) (px pt)
    onPage (Arc r p start end) =
      Arc (dx r) (px p) start end
    onPage (Line p1 p2) =
      Line (px p1) (px p2)
    onPage (Grid d p1 p2) =
      Grid (dx d) (px p1) (px p2)
    onPage (Polygon ps) =
      Polygon [px p | p <- ps]
    onPage (PolyLine ps) =
      PolyLine [px p | p <- ps]
    onPage (Text pt name) =
      Text (px pt) name
    onPage (Annotation fmt dds) = 
      Annotation fmt [
        Distantia d (PolyPoint [px p | p <- pts]) 
        | Distantia d (PolyPoint pts) <- dds]
    onPage (AnnotationR fmt r1 r2 p angle) = 
      AnnotationR fmt r1 (dx r2) (px p) angle
    onPage (AnnotationRE fmt ext r1 r2 p angle) =  
      AnnotationRE fmt (dx ext) r1 (dx r2) (px p) angle
    onPage x = error (
      "ERROR: onPage not implemented for " ++ show x )
    dx d = distOnPage d layers
    px p = ptOnPage p layers
distOnPage d layers = dist (px pt1) (px pt2)
  where
    pt1 = Point 0 0
    pt2 = Point d 0
    px pt = ptOnPage pt layers
ptOnPage pt layers = Point (scale*(x-x1)) (scale*(y-y1))
  where
    Point x y = pt
    scale = min scaleX scaleY
    scaleX = textWidth  /  width
    scaleY = textHeight / height
    textWidth  = 14.99786 
    textHeight = 25.69635
    width  = x2 - x1
    height = y2 - y1
    (Point x1 y1,Point x2 y2) = bb
    bb = originalBoundingBox layers

   --bb = (Point 582.6 177.9,Point 685.1 318.9)
   -- Textwidth/height: 14.99786 cm / 25.69635 cm


{-
(NamedSymbPos str1 str2 dir (Node pt)) 
(Named str (Node pt))
(Circle r p)
(Filled (Circle r p))
(Named str (Arc r p start end))
(Arc r p start end)
(Polygon ps)
(Line p1 p2)
(PolyLine ps)
(Named str (Line p1 p2)) 
(AnnotationR fmt r r p angle)
(AnnotationRE fmt extra r r p angle)
(Annotation fmt dds)
(Text p name)
(ArcLine elems):
  (ArcElem r start end)
  (LineElem p1 p2)
  showPoint p1 ++ " -- " ++ showPoint p2
-}

---------------------------------------------------------
{-
lines1 = linesFromEdgeList coords1 edges1
shapes1 = points1 ++ lines1

---------------------------------------------------------
edges1 = [
  "5-25-3-6-4-2-1-17-16","25-26-27-28-9","5-7-8","18-11",
  "8-9-10-11-12-13-14-15-16-18-19","2-19-20-10","15-12",
  "28-21-20","6-24-23-22-21","26-24","7-27" ]

coords1 = [
  ( 1,224.1,599.4),
  ( 2,235.6,605.8),
  ( 3,244.0,597.0),
  ( 4,238.3,607.3),
  ...
-}

coordMap pts = Map.fromList pts
--  where
--    pts = [(show n, Point x y) | (n,x,y) <- coords]

linesFromEdgeList coords edges = concat [linesFromPath [ 
  (coordMap coords) Map.! p | p <- path] | path <- paths]
  where
    paths = [splitOn "-" x | x <- edges]

linesFromPath xs = [ Line pt1 pt2
  | (pt1,pt2) <- zip xs (tail xs) ]

mkPolyLines lss = map PolyLine pss
  where 
    pss = [map mkPoint ls | ls <- lss]

data NormalLeftRight = NormalToLeft | NormalToRight 

normalVector dir r p1 p2 = 
  vectorFromAngle r (a1 dir)
  where
    a1 NormalToLeft  = t1 + halfpi
    a1 NormalToRight = t1 - halfpi
    v1 = mkVector p1 p2
    RAD t1 = angleBt axisX v1

roundingElems r turn p1 p2 p3 p4 = 
  [ LineElem p1 pStart,
    ArcElem r (RAD start) (RAD end), 
    LineElem pEnd p4 ]
  where
    pEnd = toPoint p oppositeNormal2
    pStart = toPoint p oppositeNormal1
    p = fromJust pJust
    pJust = intersection p5 p6 p7 p8
    RAD start = angleBt axisX oppositeNormal1 
    end = start + turn
    oppositeNormal2 = opposite normal2
    oppositeNormal1 = opposite normal1
    p8 = toPoint p7 v2
    v2 = mkVector p3 p4
    p7 = toPoint p3 normal2
    normal2 = normalVector dir r p3 p4
    p6 = toPoint p5 v1
    v1 = mkVector p1 p2
    p5 = toPoint p1 normal1
    normal1 = normalVector dir r p1 p2
    dir
      | turn < 0  = NormalToRight
      | otherwise = NormalToLeft

roundedLL r (LineElem p1 p2) (LineElem p3 p4) = newElems
  where
    newElems = roundingElems r turn p1 p2 p3 p4 
    RAD turn = angleBt (mkVector p1 p2) (mkVector p3 p4) 

elementCenter p (ArcElem r (RAD start) (RAD end)) = 
  toPoint p (vectorFromAngle (-r) start)

sortByDistanceFrom p pts =
  [pts | (pts,ds) <- ordered]
  where
    ordered = sortBy (compare `on` snd) ptsDs
    ptsDs = zip pts ds
    ds = [dist pt p | pt <- pts]

closestIntersection circle (Point x1 y1) (Point x2 y2) = 
  pts2 !! idx
  where
    idx = fromJust justId
    justId = elemIndex (minimum ds) ds
    ds = [dist p (Point ((x1+x2)/2) ((y1+y2)/2)) | p <- pts2]
    pts2 = circleLineIntersections circle (Point x1 y1) (Point x2 y2)

shorten r1 r2 p1 p2 = Line p3 p4
  where
    p4 = toPoint p1 v3
    p3 = toPoint p1 v2
    v3 = vectorFromAngle (d-r2) alpha
    v2 = vectorFromAngle r1 alpha
    RAD alpha = angleBt axisX v1
    v1 = mkVector p1 p2
    d = dist p1 p2

midway (Point x1 y1) (Point x2 y2) = 
  Point ((x1+x2)/2) ((y1+y2)/2)

halve (LineElem p1 p2) = 
  [LineElem p1 mid, LineElem mid p2]
  where
     mid = midway p1 p2

halve (ArcElem r start end) =
  [ArcElem r start mid, ArcElem r mid end]
  where 
    mid = RAD ((start1 + end1) / 2)
    RAD start1 = start
    RAD end1 = end

-- reversed roundedLA
roundedAL r (LineElem p1 p2) (ArcElem r1 start1 end1) =
  [ ArcElem r2 end2 (RAD newStart),
    ArcElem newr (RAD endAngle) (RAD startAngle),
    LineElem pStart newp1 ]
  where
    [ LineElem newp1 pStart,
      ArcElem newr (RAD startAngle) (RAD endAngle), 
      ArcElem r2 (RAD newStart) end2 ] = rLA
    rLA = roundedLA r (LineElem p1 p2) 
      (ArcElem r1 start1 end1) 

roundedLA r (LineElem p1 p2) (ArcElem r1 start1 end1) = 
  [ LineElem p1 pStart,
    ArcElem r (RAD startAngle) (RAD endAngle), 
    ArcElem r1 (RAD newStart) end1 ]
  where
    p4 = toPoint p3 v1
    v1 = mkVector p1 p2
    p3 = toPoint p1 normal1
    normal1 = normalVector dir r p1 p2
    dir
      | start1 < end1 = NormalToRight
      | otherwise = NormalToLeft
    p5 = elementCenter p2 (ArcElem r1 start1 end1)
    c2 = Circle (r1+r) p5  
    pts2 = circleLineIntersections c2 p3 p4
    p = head (sortByDistanceFrom (midway p3 p4) pts2)
    pToStart = mkVector p3 p1
    RAD startAngle = angleBt axisX pToStart
    pStart = toPoint p pToStart
    RAD d = angleBt pToStart (mkVector p p5)
    endAngle = startAngle + d
    pToEnd = vectorFromAngle r endAngle
    pEnd = toPoint p pToEnd
    newNormal = mkVector p5 p
    p8 = toPoint pEnd newNormal
    newStart = closestFullTurn newStart1 radStart1
    RAD radStart1 = start1
    RAD newStart1 = angleBt axisX newNormal

closestFullTurn alpha beta = b1
  where 
    (a1,b1) = head s
    s = sort [(abs (alpha+a-beta),alpha+a) 
      | a <- fullturns]
    fullturns = [twopi * t | t <- [-3,-2,-1,0,1,2,3]]

rounded r (Closed (ArcLine elems)) = ArcLine xs
  where
    xs = concat xss
    xss = [ roundedElem r e f | [e,f] <- chunksOf 2 
      (tail halves ++ [head halves]) ]
    halves = concat [halve elem | elem <- elems]

rounded r (ArcLine elems) = ArcLine xs
  where
    xs = [head halves] ++ concat xss ++ [last halves]
    xss = [ roundedElem r e f | [e,f] <- chunksOf 2 
      (tail halves) ] 
    halves = concat [halve elem | elem <- elems]

roundedElem r (LineElem p1 p2) (LineElem p3 p4) = 
  roundedLL r (LineElem  p1 p2) (LineElem p3 p4)

roundedElem r (LineElem p1 p2) (ArcElem r1 start1 end1) = 
  roundedLA r (LineElem p1 p2) (ArcElem r1 start1 end1) 

roundedElem r (ArcElem r1 start1 end1) (LineElem p1 p2) = 
  roundedAL r (LineElem p2 p1) (ArcElem r1 end1 start1)

roundedElem r e f = [e,f]

data GrabType = Tangents | Quads | Triads

explodeC Tangents (Circle r1 p1) (Circle r2 p2) =
  [Arc r1 p1 t1 t2, Arc r1 p1 t2 t3,
   Arc r2 p2 t1 t2, Arc r2 p2 t2 t3]
  where
    (delta,thetas) = circTangAngles r1 p1 r2 p2
    [th1,th2] = sort thetas
    [t1,t2,t3] = map RAD [th1,th2,th1+twopi]

tangents (Circle r1 p1) (Circle r2 p2)  
  | r2 >= r1  = circTangents r1 p1 r2 p2
  | otherwise = circTangents r2 p2 r1 p1

circTangAngles1 (Circle r1 p1) (Circle r2 p2)  = 
  circTangAngles r1 p1 r2 p2 

circTangAngles r1 p1 r2 p2 = 
  (delta,thetas)
  where
    d = dist p1 p2
    r3 = r2 - r1
    theta = acos (r3 / d)
    v1 = mkVector p2 p1
    RAD delta = angleBt axisX v1
    thetas = map (\p -> p delta theta) [(+),(-)]

circTangents r1 p1 r2 p2 = 
  [Line (ps!!0) (ps!!2),Line (ps!!1) (ps!!3)]
  where
    (delta,thetas) = circTangAngles r1 p1 r2 p2 
    vs = [vectorFromAngle r t | r <- [r2,r1], t <- thetas]
    ps = [toPoint p v | (p,v) <- zip [p2,p2,p1,p1] vs]

data AnnotDir a b = AnnotToLeft a b 
  | AnnotToUp a b 
  | AnnotToRight a b 
  | AnnotToDown a b

annotate fmt (AnnotToLeft  x pairs) = 
  Annotation fmt 
  [Distantia t (polypoint1 x p) 
  | (p,t) <- zip pairs (distancesY pairs)]
annotate fmt (AnnotToUp    y pairs) = 
  Annotation fmt 
  [Distantia t (polypoint2 y p) 
  | (p,t) <- zip pairs (distancesX pairs)]
annotate fmt (AnnotToRight x pairs) = 
  Annotation fmt [Distantia t (polypoint3 x p) 
  | (p,t) <- zip pairs (distancesY pairs)]
annotate fmt (AnnotToDown  y pairs) = 
  Annotation fmt 
  [Distantia t (polypoint4 y p) 
  | (p,t) <- zip pairs (distancesX pairs)]

distancesY pairs = [0] ++ [snd pair2 - snd pair1 
  | (pair1,pair2) <- zip pairs (tail pairs)]
distancesX pairs = [0] ++ [fst pair2 - fst pair1 
  | (pair1,pair2) <- zip pairs (tail pairs)]

polypoint1 x (x1,y1) = PolyPoint [a,b,c] 
  where
    a = Point (x1-2) y1
    b = Point x y1
    c = Point (x-2) y1

polypoint2 y (x1,y1) = PolyPoint [a,b,c] 
  where
    a = Point x1 (y1+2)
    b = Point x1 y
    c = Point x1 (y+2)

polypoint3 x (x1,y1) = PolyPoint [a,b,c] 
  where
    a = Point (x+2) y1
    b = Point x y1
    c = Point (x1+2) y1

polypoint4 y (x1,y1) = PolyPoint [a,b,c] 
  where
    a = Point x1 (y1-2)
    b = Point x1 y
    c = Point x1 (y-2)

annotRadius fmt angle (Circle r p) = 
  AnnotationR fmt r r p angle
annotRadiusE fmt extra angle (Circle r p) = 
  AnnotationRE fmt extra r r p angle

data Rect = Rect Point Point

fromRect (Rect p1 p2) = Polygon [Point x y | (x,y) <- r1]
  where
    r1 = [(x1,y1),(x1,y2),(x2,y2),(x2,y1)] 
    Point x1 y1 = p1
    Point x2 y2 = p2

data PolyPoint = PolyPoint [Point]
  deriving Show

data Distantia = Distantia Double PolyPoint
  deriving Show

data ArcLineElem = ArcElem Double Angle Angle
  | LineElem Point Point
  deriving Show

drawTikz (NamedSymbPos str1 str2 dir (Node pt)) n = 
  "\\node (" ++ n ++ ") at (" ++ show x ++ "," ++ show y ++ 
    ") {" ++ str2 ++ "};\n" ++
    "\\node[" ++ tikzDirection dir ++ "] at (" ++ n ++ ") {" ++ str1 ++ "};\n"
  where   
    Point x y = pt

drawTikz (Named str (Node pt)) n = 
  drawTikz (NamedSymbPos str "$\\times$" NE (Node pt)) n 

drawTikz (Circle r p) n = tikzPoint n p ++ tikzCircle n (show r)
drawTikz (Filled (Circle r p)) n = tikzPoint n p ++ tikzFilledCircle n (show r)

drawTikz (Named str (Arc r p start end)) n = tikzPoint n p ++ 
  tikzNamedArc str n (show r) s e
  where 
    [s,e] = map (showdeg . degreesNonMod) [start,end]

drawTikz (Arc r p start end) n = tikzPoint n p ++ 
  tikzArc n (show r) s e
  where 
    [s,e] = map (showdeg . degreesNonMod) [start,end]
drawTikz (Polygon ps) n = tikzPolygon n ps
drawTikz (Line p1 p2) n = tikzPolyline n [p1,p2]
drawTikz (Grid d p1 p2) n = 
  "\\draw[step=" ++ show d ++ "] (" ++ show x1 ++ "," ++
  show y1 ++ ") grid ("++ show x2 ++ "," ++ show y2 ++ 
  ");"
  where   
    Point x1 y1 = p1
    Point x2 y2 = p2

drawTikz (PolyLine ps) n = tikzPolyline n ps
drawTikz (Named str (Line p1 p2)) n = 
  "\\draw[-latex] (" ++ show x1 ++ "," ++ show y1 ++ 
    ") -- node[midway,sloped,above] (" ++ name1 ++ ") {\\phantom{$X_m$}} (" ++
    show x2 ++ "," ++ show y2 ++ ");\n" ++
    "\\node at (" ++ name1 ++ ") {" ++ str ++ "};\n"
  where   
    Point x1 y1 = p1
    Point x2 y2 = p2
    name1 = n ++ "-label"

drawTikz (AnnotationR fmt r1 r2 p angle) n = 
  tikzPoint name1 (p) ++ 
  tikzPoint name2 (twr r2) ++
  arrow name1 name2 r1
  where
    name1 = n ++ "-1"
    name2 = n ++ "-2"
    twr = towards angle p
    arrow n1 n2 r1 = "\\draw [->,tri] (" ++ n1 ++ 
      ") -- node[pos=1.0,above " ++ lr ++ "=0pt and 8pt,sloped] {" ++ 
      printf fmt r1 ++
      "}  (" ++ n2 ++ ");\n"
    lr | angle < (DEG 90) = "left"
       | angle > (DEG 270) = "left"
       | otherwise = "right"
-- 'above right' should be 'above left', when angle < 90 or angle > 270
drawTikz (AnnotationRE fmt extra r1 r2 p angle) n = tikzPoint name1 (p) ++ 
  tikzPoint name2 (twr r2) ++ tikzPoint name3 (twr extra) ++
  arrow1 name1 name2 ++ arrow2 name2 name3 r1
  where
    name1 = n ++ "-1"
    name2 = n ++ "-2"
    name3 = n ++ "-3"
    twr = towards angle p
    arrow1 n1 n2 = "\\draw [->,tri] (" ++ n1 ++ 
      ") -- (" ++ n2 ++ ");\n"
    arrow2 n1 n2 r1 = "\\draw (" ++ n1 ++ 
      ") -- node[pos=1.0,above " ++ lr ++ ",sloped] {" ++ 
      printf fmt r1 ++
      "}  (" ++ n2 ++ ");\n"
    lr | angle < (DEG 90) = "left"
       | angle > (DEG 270) = "left"
       | otherwise = "right"

drawTikz (Annotation fmt dds) n = 
  concat (map (\(pp,k,d) -> (tikzPolyline (name k) pp) ++ twoTipArrow n k d) 
    ([(toPointsPP pp,k,d) | (Distantia d pp,k) <- zip dds [1..]])) 
  where
    name k = n ++ "-" ++ show k
    twoTipArrow n 1 d = ""
    twoTipArrow n k d = "\\draw [<->,tri] (" ++ n ++ "-" ++ 
      show (k-1) ++ "-2) -- node[above,sloped] {" ++ 
      printf fmt d ++"}  (" ++ n ++ "-" ++ 
      show k ++ "-2);\n"

drawTikz (Text p name) n = tikzPoint n p ++ 
  "\\node[right,xshift=1.5cm] at (" ++ n ++") {\\texttt{" ++ name ++ "}};\n"

drawTikz (ArcLine elems) n =
  "\\draw "++ i ++ ";\n"
  where
    i = replace "-- arc" "arc" i1
    i1 = intercalate " -- " elemsStr
    elemsStr = [tikzAE e | e <- elems]

tikzAE (ArcElem r start end) = 
 "arc (" ++ s ++ ":" ++ e ++ ":" ++ show r ++ ")"
  where 
    [s,e] = map (showdeg . degreesNonMod) [start,end]

tikzAE (LineElem p1 p2) = 
  showPoint p1 ++ " -- " ++ showPoint p2
 
showPoint (Point x y) = 
  "(" ++ show x ++
  "," ++ show y ++
  ")"

towards a p r = Point x2 y2
  where
    Point x1 y1 = p
    (x2,y2) = (x1 + r * cos1 a, y1 + r * sin1 a)

tikzSmallCircles n ps = concat [tikzPoint k p ++ tikzCircle k "0.2" | (p,k) <- zip ps names]
  where
    names = [(n ++ "-" ++ show k) | k <- [1..]]

tikzPolyline n ps = concat [tikzPoint k p | (p,k) <- zip ps names] ++ path
  where
    names = [(n ++ "-" ++ show k) | k <- [1..]]
    path = tikzPl [k | (p,k) <- zip ps names]

tikzPl t = draw t
  where
    draw t1 = "\\draw (" ++ path t1 ++ ");\n"
    path t1 = intercalate ") -- (" t1

tikzPolygon n ps = concat [tikzPoint k p | (p,k) <- zip ps names] ++ path
  where
    names = [(n ++ "-" ++ show k) | k <- [1..]]
    path = tikzPg [k | (p,k) <- zip ps names]

tikzPg t = (draw . closed) t
  where
    --closed t2 = t2 ++ [head t2]
    closed t2 = t2 
    --draw t1 = "\\draw (" ++ path t1 ++ ");\n"
    draw t1 = "\\draw (" ++ path t1 ++ ") -- cycle;\n"
    path t1 = intercalate ") -- (" t1

tikzPoint name (Point x y) =
  "\\coordinate (" ++ name ++
  ") at (" ++ show x ++
  "," ++ show y ++
  ");\n"

tikzFilledCircle n r = 
 "\\fill (" ++ n ++ ") circle (" ++ r ++ ");\n"

tikzCircle n r = 
 "\\draw (" ++ n ++ ") circle (" ++ r ++ ");\n"

tikzArc n r start end = 
 "\\draw ([shift=(" ++ start ++ ":" ++ r ++ ")]" ++ n ++ 
    ") arc (" ++ start ++ ":" ++ end ++ ":" ++ r ++ ");\n"

tikzNamedArc str n r start end = 
  "\\draw[-latex,shorten <=2pt] ([shift=(" ++ start ++ ":" ++ r ++ ")]" ++ n ++ 
    ") arc (" ++ start ++ ":" ++ end ++ ":" ++ r ++ ")" ++
    " node[midway,sloped,above] (" ++ name1 ++ ") {\\phantom{$X_m$}};\n" ++
    "\\node at (" ++ name1 ++ ") {" ++ str ++ "};\n"
  where   
    name1 = n ++ "-label"


-- scale=1.0 always
-- Textwidth/height: 14.99786 cm / 25.69635 cm
prePict = "\\begin{tikzpicture}\n\\tikzstyle{tri} = [>=triangle 45]\n"
postPict = "\n\\end{tikzpicture}\n"




