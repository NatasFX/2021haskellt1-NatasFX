module Main where

import DeepZoom (deepFrame, deepPoint, precisionBitsFor)
import AudioTiming (getAnimationTimings, legacyAnimationTimings)
import Data.WAVE (getWAVEFile, waveFrames, waveFrameRate, waveHeader, waveSamples)
import System.Exit (exitFailure)
import System.CPUTime (getCPUTime)

main :: IO ()
main = do
  assert "guard bits grow with zoom" (precisionBitsFor 1.0e100 > 300)
  assert "the origin does not escape at deep zoom"
    (fst (deepPoint (deepFrame 10 10 100 1.0e100 "0" "0") 5 5) == 100)
  assert "an escaping reference point retains its iteration count"
    (fst (deepPoint (deepFrame 10 10 100 1.0e100 "2" "0") 5 5) == 2)
  compareAudioTimings

assert :: String -> Bool -> IO ()
assert label condition =
  if condition
    then putStrLn ("ok: " ++ label)
    else putStrLn ("failed: " ++ label) >> exitFailure

compareAudioTimings :: IO ()
compareAudioTimings = do
  wave <- getWAVEFile "mandeloso.wav"
  let header = waveHeader wave
  let samples = [(fromIntegral (head channel) - 32768) / 32768 | channel <- waveSamples wave]
  let sampleRate = waveFrameRate header
  let shiftedSamples = drop (sampleRate `div` 9) samples
  let duration = maybe 0 (`div` sampleRate) (waveFrames header)
  let framesPerSecond = 144
  let samplesPerFrame = sampleRate `div` framesPerSecond
  let allFrameCount = (length samples * framesPerSecond + sampleRate - 1) `div` sampleRate
  -- The legacy envelope is quadratic.  2,048 frames (~14 seconds at 144 fps)
  -- are enough to characterize a sustained run without making the test suite slow.
  let frames = [0..min 2048 allFrameCount]
  beforeFast <- getCPUTime
  let fast = getAnimationTimings 40000 shiftedSamples frames samplesPerFrame duration
  forceTimings fast `seq` pure ()
  afterFast <- getCPUTime
  beforeLegacy <- getCPUTime
  let legacy = legacyAnimationTimings 40000 shiftedSamples frames samplesPerFrame duration
  forceTimings legacy `seq` pure ()
  afterLegacy <- getCPUTime
  assert "optimized WAV timings exactly match legacy timings at 144 fps" (fast == legacy)
  let oldOffset = allFrameCount * samplesPerFrame
  let exactOffset = (allFrameCount * sampleRate + framesPerSecond `div` 2) `div` framesPerSecond
  assert "integer samples-per-frame would visibly drift on the full track" (exactOffset - oldOffset > sampleRate `div` 10)
  putStrLn ("audio timing CPU: fast=" ++ show (afterFast - beforeFast) ++ "ps legacy=" ++ show (afterLegacy - beforeLegacy) ++ "ps")

forceTimings :: [((Int, Int), Int)] -> ()
forceTimings = foldr (\((frame, bass), hue) rest -> frame `seq` bass `seq` hue `seq` rest) ()
