{-# LANGUAGE BangPatterns #-}

-- |
-- High precision reference orbits for deep Mandelbrot zooms.
--
-- Coordinates are stored as fixed-point integers while the reference orbit is
-- built.  Pixels are then evaluated with perturbation theory in 'Double'.
-- This keeps the expensive arbitrary-precision work to one orbit per frame.
module DeepZoom
  ( DeepFrame
  , deepFrame
  , deepPoint
  , deepReferenceOrbit
  , precisionBitsFor
  ) where

import Data.Bits (shiftL)
import Data.Char (isDigit)
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CDouble)

data FixedComplex = FixedComplex !Integer !Integer !Int

data DeepFrame = DeepFrame
  { deepWidth :: !Int
  , deepHeight :: !Int
  , deepZoom :: !Double
  , deepBits :: !Int
  , deepOrbit :: ![(Double, Double, Double, Double)]
  }

-- | Packed hi/lo reference orbit for the CUDA perturbation kernel.
deepReferenceOrbit :: DeepFrame -> VS.Vector CDouble
deepReferenceOrbit = VS.fromList . concatMap pack . deepOrbit
  where
    pack (realHi, realLo, imaginaryHi, imaginaryLo) =
      [realToFrac realHi, realToFrac realLo, realToFrac imaginaryHi, realToFrac imaginaryLo]

-- Keep 64 guard bits beyond the detail represented by a pixel.  A minimum is
-- useful at the hand-off from the ordinary Double renderer.
precisionBitsFor :: Double -> Int
precisionBitsFor zoom = max 128 (ceiling (logBase 2 zoom) + 64)

-- | Build a frame-specific reference orbit from decimal strings.  Keeping the
-- source coordinates as strings is important: a Double literal would already
-- have lost the information that deep zoom needs.
deepFrame :: Int -> Int -> Int -> Double -> String -> String -> DeepFrame
deepFrame width height iterations zoom centerX centerY =
  DeepFrame width height zoom bits (map toDouble orbit)
  where
    bits = precisionBitsFor zoom
    center = FixedComplex (fixedFromDecimal bits centerX) (fixedFromDecimal bits centerY) bits
    orbit = FixedComplex 0 0 bits : take iterations (tail (iterate (step center) (FixedComplex 0 0 bits)))

-- | Escape iteration for one pixel.  The reference z is high precision; the
-- perturbation dz is small and therefore fast in hardware Double arithmetic:
--
--   dz(n+1) = 2 * zRef(n) * dz(n) + dz(n)^2 + dc
deepPoint :: DeepFrame -> Int -> Int -> (Int, Double)
deepPoint frame x y = go 0 (0, 0) (deepOrbit frame)
  where
    !deltaC = pixelDelta frame x y

    go !iteration !delta (reference : nextReference : rest) =
      let !nextDelta = add (add (scaleReference 2 reference delta) (multiply delta delta)) deltaC
          !point = add (referenceValue nextReference) nextDelta
          !magnitudeSquared = squaredMagnitude point
       in if isNaN magnitudeSquared || isInfinite magnitudeSquared
            then (iteration + 1, 4)
            else if magnitudeSquared >= 16
              then (iteration + 1, sqrt magnitudeSquared)
            else go (iteration + 1) nextDelta (nextReference : rest)
    go !iteration !delta (reference : _) = (iteration, sqrt (squaredMagnitude (add (referenceValue reference) delta)))
    go !iteration !delta [] = (iteration, sqrt (squaredMagnitude delta))

pixelDelta :: DeepFrame -> Int -> Int -> (Double, Double)
pixelDelta frame x y =
  (scaledOffset (fromIntegral x - fromIntegral (deepWidth frame) / 2),
   scaledOffset (fromIntegral y - fromIntegral (deepHeight frame) / 2))
  where
    -- The factor remains close to 2^64 even at extreme zooms, so this does
    -- not discard the small pixel offset before it reaches the fixed point.
    !factor = 2 ** (fromIntegral (deepBits frame) - logBase 2 (deepZoom frame))
    scaledOffset offset = encodeFloat (round (offset * factor) :: Integer) (negate (deepBits frame))

step :: FixedComplex -> FixedComplex -> FixedComplex
step (FixedComplex cr ci bits) (FixedComplex zr zi _) =
  FixedComplex
    (mulScaled bits zr zr - mulScaled bits zi zi + cr)
    -- 'mulScaled' expects both operands in fixed-point form.  Applying it
    -- again to the integer literal 2 divided this term by 2^bits a second
    -- time, effectively erasing the imaginary reference orbit.
    (2 * mulScaled bits zr zi + ci)
    bits

mulScaled :: Int -> Integer -> Integer -> Integer
mulScaled bits a b = (a * b) `quot` (1 `shiftL` bits)

fixedFromDecimal :: Int -> String -> Integer
fixedFromDecimal bits value = sign * (numerator * (1 `shiftL` bits) `quot` denominator)
  where
    (sign, unsigned) = case value of
      '-' : rest -> (-1, rest)
      '+' : rest -> (1, rest)
      _ -> (1, value)
    (whole, fractionalWithDot) = break (== '.') unsigned
    fractional = drop 1 fractionalWithDot
    digits = filter isDigit (whole ++ fractional)
    numerator = if null digits then 0 else read digits
    denominator = 10 ^ length fractional

-- Keep the residual discarded by a single Double conversion.  At deep zoom it
-- is the same order as one pixel's delta and is therefore essential to a
-- stable perturbation orbit.
toDouble :: FixedComplex -> (Double, Double, Double, Double)
toDouble (FixedComplex real imaginary bits) =
  let (realHi, realLo) = split real bits
      (imaginaryHi, imaginaryLo) = split imaginary bits
   in (realHi, realLo, imaginaryHi, imaginaryLo)
  where
    split value precision =
      let high = encodeFloat value (negate precision)
          (mantissa, exponent) = decodeFloat high
          shift = exponent + precision
          highFixed = if shift >= 0 then mantissa `shiftL` shift else mantissa `quot` (1 `shiftL` negate shift)
       in (high, encodeFloat (value - highFixed) (negate precision))

referenceValue :: (Double, Double, Double, Double) -> (Double, Double)
referenceValue (realHi, realLo, imaginaryHi, imaginaryLo) = (realHi + realLo, imaginaryHi + imaginaryLo)

scaleReference :: Double -> (Double, Double, Double, Double) -> (Double, Double) -> (Double, Double)
scaleReference factor (realHi, realLo, imaginaryHi, imaginaryLo) (deltaReal, deltaImaginary) =
  (factor * ((realHi * deltaReal - imaginaryHi * deltaImaginary) + (realLo * deltaReal - imaginaryLo * deltaImaginary)),
   factor * ((realHi * deltaImaginary + imaginaryHi * deltaReal) + (realLo * deltaImaginary + imaginaryLo * deltaReal)))

add :: (Double, Double) -> (Double, Double) -> (Double, Double)
add (ar, ai) (br, bi) = (ar + br, ai + bi)

scale :: Double -> (Double, Double) -> (Double, Double)
scale n (real, imaginary) = (n * real, n * imaginary)

multiply :: (Double, Double) -> (Double, Double) -> (Double, Double)
multiply (ar, ai) (br, bi) = (ar * br - ai * bi, ar * bi + ai * br)

squaredMagnitude :: (Double, Double) -> Double
squaredMagnitude (real, imaginary) = real * real + imaginary * imaginary
