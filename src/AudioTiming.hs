{-# LANGUAGE BangPatterns #-}

-- | FFT-derived timing data for the animation.  The fast path is deliberately
-- equivalent to 'legacyAnimationTimings', retained for characterization tests.
module AudioTiming
  ( getAnimationTimings
  , getAnimationTimingsAt
  , legacyAnimationTimings
  ) where

import Data.Array.CArray (CArray, listArray)
import Data.Array.IArray (elems)
import Data.Complex (Complex, magnitude)
import Data.List (sort)
import qualified Data.Vector.Unboxed as V
import Math.FFT (dftRC)

fftWindow :: Int
fftWindow = 1025

carray :: [Double] -> CArray (Int, Int) Double
carray = listArray ((0, 0), (fftWindow - 1, 0))

cleanComplex :: CArray (Int, Int) (Complex Double) -> Int
cleanComplex c = sum (take 7 cleaned) `div` 10
  where
    cleaned = map (\component -> round (magnitude component) `div` 100) (elems c)

normalizeBass :: Int -> [Int] -> [Int]
normalizeBass target values
  | reference <= 0 = replicate (length values) 0
  | otherwise = map (min target . scale) values
  where
    ordered = sort values
    reference = ordered !! ((length ordered - 1) * 95 `div` 100)
    scale value = value * target `div` reference

-- | O(frames * fftWindow): vector indexing makes each audio window bounded,
-- and both envelope stages are strict incremental scans.
getAnimationTimings :: Int -> [Double] -> [Int] -> Int -> Int -> [((Int, Int), Int)]
getAnimationTimings bassTarget samples rangeFrames samplesPerFrame duration =
  getAnimationTimingsAt bassTarget samples rangeFrames (\frame -> samplesPerFrame * frame + 1) duration

-- | Variant with an exact frame-to-sample clock.  A rounded integer number of
-- samples per frame drifts whenever sampleRate is not divisible by FPS (for
-- example 44,100 / 144 = 306.25).
getAnimationTimingsAt :: Int -> [Double] -> [Int] -> (Int -> Int) -> Int -> [((Int, Int), Int)]
getAnimationTimingsAt bassTarget samples rangeFrames sampleOffset duration =
  zipWith (\(frame, bass) hue -> ((frame, bass), hue)) frameBass hueValues
  where
    sampleVector = V.fromList samples
    bassValues = normalizeBass bassTarget (map bassAt rangeFrames)
    bassAt frame =
      cleanComplex . dftRC . carray $ windowAt sampleVector (sampleOffset frame)

    frameNumbers = [2..last rangeFrames]
    smooth previous current = previous - (previous - current) `div` 4
    smoothed = take (length frameNumbers) (tail (scanl smooth (head bassValues) (tail bassValues)))
    frameBass = zip frameNumbers smoothed

    addHue total bass = total + bass `div` 15
    hueValues = case smoothed of
      first : second : rest ->
        let prefix = scanl addHue (first + second `div` 15) rest
         in prefix ++ [last prefix]
      _ -> []

windowAt :: V.Vector Double -> Int -> [Double]
windowAt samples offset = V.toList present ++ replicate (fftWindow - V.length present) 0
  where
    present = V.take fftWindow (V.drop offset samples)

-- | The original list-based implementation, used only to prove that the
-- optimized version preserves the animation's existing timing values.
legacyAnimationTimings :: Int -> [Double] -> [Int] -> Int -> Int -> [((Int, Int), Int)]
legacyAnimationTimings bassTarget samples rangeFrames samplesPerFrame duration =
  map makeTiming newState
  where
    bassValues = normalizeBass bassTarget (map bassAt rangeFrames)
    bassAt frame = cleanComplex . dftRC . carray $ takeWindow (drop (samplesPerFrame * frame + 1) samples)
    newState = map smoothState [2..last rangeFrames]
    smoothState size = (size, foldl1 smooth (take size bassValues))
    smooth previous current = previous - (previous - current) `div` 4
    makeTiming (size, bass) =
      ((size, bass), foldl1 addHue (take size [value | (_, value) <- newState]))
    addHue total bass = total + bass `div` 15

takeWindow values = take fftWindow values ++ replicate (max 0 (fftWindow - length values)) 0
