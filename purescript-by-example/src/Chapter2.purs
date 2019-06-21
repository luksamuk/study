module Chapter2 where

import Math (sqrt, pi)
import Prelude

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r
