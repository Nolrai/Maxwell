module Globe where

import Linear.V2
import Linear.V3

inverseMollweide :: Double -> V2 Double -> V2 Double
inverseMollweide r (V2 x y) = V2 phi lambda
  where
    phi = asin ((2 * theta + sin (2 * theta))/ pi)
    lambda = pi * x / (2 * r * sqrt 2 * cos theta)
    theta = asin (y / r * sqrt 2)

sphericalToCartesian :: V3 Double -> V3 Double
sphericalToCartesian (V3 radial azimuthal polar) = V3 x y z
  where
    x = radial * cos azimuthal * sin polar
    y = radial * sin azimuthal * sin polar
    z = radial * cos polar

phiLambdaToSperical :: Double -> V2 Double -> V3 Double
phiLambdaToSperical radial (V2 phi lambda) = V3 radial azimuthal polar
  where
    azimuthal = lambda
    polar = (pi / 2) - phi

inverseProjection :: Double -> V2 Double -> V3 Double
inverseProjection r = sphericalToCartesian . phiLambdaToSperical r . inverseMollweide r