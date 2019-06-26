module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Global as Global
import Math (pi)
import Math as Math


-- Algebraic data types
data Shape
  = Circle    Point Number
  | Rectangle Point Number Number
  | Line      Point Point
  | Text      Point String
    -- Exercise 5.17.2, part 1
  | Clipped   Shape Picture

data Point =
  Point { x :: Number
        , y :: Number }

-- Stuff reimplemented, just for kicks
-- data Option a = None | Some a
-- data List     = Nil  | Cons a (List a)

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }

    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }


showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x  <> ", " <> show y <> ")"
-- showPoint (Point { x: x, y: y }) =


showShape :: Shape -> String
showShape (Circle c r) =
  "Circle { c: " <> showPoint c <> ", r: " <> show r <> " }"
showShape (Rectangle c w h) =
  "Rect { c: " <> showPoint c <> ", size: " <> show w <> "x" <> show h <> " }"
showShape (Line start end) =
  "Line { " <> showPoint start <> " -- " <> showPoint end <> " }"
showShape (Text p text) =
  "Text { \"" <> show text <> " @ " <> showPoint p <> " }"
  -- Exercise 5.17.2, part 2
showShape (Clipped s p) =
  "Clipped [ " <> (showAll <<< showPicture) p <> " ] into { " <> showShape s
  where
    showAll :: Array String -> String
    showAll = foldl (<>) ""


origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0



-- Exercise 5.14.1
theCircle :: Shape
theCircle = (Circle c r)
  where
    c = origin
    r = 10.0

-- Exercise 5.14.2
doubleScale :: Shape -> Shape
doubleScale (Circle c r)      = Circle c (r * 2.0)
doubleScale (Rectangle c w h) = Rectangle c (w * 2.0) (h * 2.0)
doubleScale x                 = x

-- Exercise 5.14.3
extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _             = Nothing


newtype Pixels = Pixels Number
newtype Inches = Inches Number


type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture p = map showShape p

data Bounds =
  Bounds { top    :: Number
         , left   :: Number
         , bottom :: Number
         , right  :: Number }

emptyBounds :: Bounds
emptyBounds = Bounds { top, left, bottom, right }
  where
    top    = 0.0
    left   = 0.0
    bottom = 0.0
    right  = 0.0


shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) =
  Bounds { top:    y + r,
           left:   x - r,
           bottom: y - r,
           right:  x + r  }
shapeBounds (Rectangle (Point { x, y }) w h) =
  Bounds { top:    y + half_height,
           left:   x - half_width,
           bottom: y - half_height,
           right:  x + half_width  }
  where
    half_height = h / 2.0
    half_width  = w / 2.0
shapeBounds (Line
             (Point { x: startx, y: starty })
             (Point { x: endx, y: endy }))    =
  Bounds { top:    greatest_y,
           left:   smallest_x,
           bottom: smallest_y,
           right:  greatest_x  }
  where
    smallest_x = min startx endx
    greatest_x = max startx endx
    smallest_y = min starty endy
    greatest_y = max starty endy
shapeBounds (Text (Point { x, y }) _) =
  Bounds { top:    y,
           left:   x,
           bottom: y,
           right:  x  }
  -- Exercise 5.17.2, part 3
shapeBounds (Clipped shape picture) =
  clipBounds (shapeBounds shape) (bounds picture)
  where
    clipBounds :: Bounds -> Bounds -> Bounds
    clipBounds constraint@(Bounds { top: tc, left: lc, bottom: bc, right: rc })
               target@(Bounds { top: tt, left: lt, bottom: bt, right: rt }) =
      Bounds { top:    smallest_top,
               left:   greatest_left,
               bottom: greatest_bottom,
               right:  smallest_right  }
      where
        smallest_top    = min tc tt
        greatest_left   = max lc lt
        greatest_bottom = max bc bt
        smallest_right  = min rc rt


showBounds :: Bounds -> String
showBounds (Bounds { top, left, bottom, right }) =
  "Bounds { (" <> show left <> ", " <> show bottom <>
  ") -> (" <> show right <> ", " <> show top <> ") }"


bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
    combine :: Bounds -> Shape -> Bounds
    combine b shape = union (shapeBounds shape) b

    union :: Bounds -> Bounds -> Bounds
    union (Bounds { top: t1, left: l1, bottom: b1, right: r1 })
          (Bounds { top: t2, left: l2, bottom: b2, right: r2 }) =
      Bounds { top:    greatest_top,
               left:   smallest_left,
               bottom: smallest_bottom,
               right:  greatest_right   }
      where
        greatest_top    = max t1 t2
        smallest_left   = min l1 l2
        smallest_bottom = min b1 b2
        greatest_right  = max r1 r2



-- Exercise 5.17.1
-- Text area is assumed 0.0, lines have no area
area :: Shape -> Number
area (Circle _ r)      = pi * r * r
area (Rectangle _ w h) = w * h
area x                 = 0.0

