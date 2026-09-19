{-# LANGUAGE BangPatterns #-}

{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
import Text.Printf ( printf )
import System.IO
import System.Exit ( ExitCode(..), exitFailure, exitSuccess )
import Data.WAVE
import Data.Array.CArray
import qualified Data.Array.IArray
import Data.Complex
import Data.Maybe ( fromMaybe )
import Data.Char ( toLower )
import System.Process ( StdStream(CreatePipe), callCommand, createProcess, proc, std_in, waitForProcess )
import DeepZoom ( DeepFrame, deepFrame, deepPoint, deepReferenceOrbit )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Concurrent ( MVar, forkOn, newEmptyMVar, putMVar, takeMVar )
import Control.Exception ( AsyncException(UserInterrupt), SomeException, catch, mask, throwIO, try )
import Control.Monad ( forM, forM_, when )
import Data.IORef ( atomicModifyIORef', newIORef )
import GHC.Conc ( getNumCapabilities )
import AudioTiming ( getAnimationTimingsAt )
import CudaRender ( cudaRenderDeepFrame, cudaRenderFrame )


type CComplex = (Double, Double)


-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
musicPath, videoPath, imagesPath :: FilePath
width, height, maxIter, hueOffset, sensitivity, dropped :: Int
bassTarget :: Int
coordY, coordX, maxZoom, framerate, fixedZoom :: Double
coordXText, coordYText :: String

musicPath = "./mandeloso.wav"
-- Matroska is streamable while FFmpeg is still receiving frames; unlike MP4,
-- it does not need a final index before VLC/mpv can begin playback.
videoPath = "./mandelbrot.mkv"
imagesPath = "./anim/%d.png"

width = 1920          -- largura
height = 1080        -- altura
maxIter = 64        -- Zoom raso não precisa do orçamento de um deep zoom.
hueOffset = -150       -- caso queira que a cor inicial seja diferente 

maxZoom = 1.0e300     -- O deep zoom usa orbita de referencia; Double deixa de limitar o centro.

coordXText = "0.36024044343761436323612524444954530848260780795858575048837581474019534605"
coordYText = "-0.64131306106480317486037501517930206657949495228230525955617754306444857417"
coordX = read coordXText  -- Caminho Double para zooms rasos.
coordY = read coordYText

framerate = 60        -- Framerate do vídeo

sensitivity = 20      -- Sensibilidade ao grave maior = menos sensível

-- O percentil 95% do grave é mapeado para este valor; picos acima dele são
-- limitados antes de alimentar a resposta acumulativa de zoom e paleta.
bassTarget = 40000

estático :: Bool
estático = False       -- Quando True ele não fará zoom e se manterá estático em fixedZoom

fixedZoom = 17.5        -- Zoom caso estático for True

dropped = 0       -- Se quiser retomar uma animação pelo numero da imagem, use aqui

-----------------------------------------------------------
-- Funções diversas
-----------------------------------------------------------
fst3, snd3, thr :: (a, a, a) -> a
fst3 (x, _, _) = x
snd3 (_, x, _) = x
thr (_, _, x)  = x

genPath :: Int -> FilePath
genPath = printf imagesPath

exitFalha :: String -> IO ()
exitFalha s = do
  a <- putStrLn s
  exitFailure

toFloat :: Int -> Double
toFloat i = fromIntegral i :: Double

status :: Int -> Int -> IO ()
status total n = do
  putStr (printf "\r%.f%%\t- %d/%d Frames" (toFloat n/toFloat total*100) n total)


-----------------------------------------------------------
-- Criação de paletas de cor
-----------------------------------------------------------
filterPallete :: [(Double, Double, Double)] -> [(Pixel8,Pixel8,Pixel8)]
filterPallete = map (\(r,g,b) -> (round r, round g, round b))

laranja, azul, roxo, verde :: [(Pixel8, Pixel8, Pixel8)]
laranja = filterPallete $ zip3 [0..254.0] [0,0.72..184] (replicate 254 0.0)

verde = filterPallete $ zip3 [0,0.46..120] [0..255.0] (replicate 254 0.0)

azul = filterPallete $ zip3 (replicate 254 0.0) (replicate 254 0.0) [0..255.0]

roxo = filterPallete $ zip3 [0..254] (replicate 254 0.0) [0,0.59..150.0]

pallete :: [(Pixel8,Pixel8,Pixel8)]
pallete = azulInOut ++ laranjaInOut ++ verdeInOut ++ roxoInOut
  where laranjaInOut = laranja ++ reverse laranja
        verdeInOut = verde ++ reverse verde
        azulInOut  = azul ++ reverse azul
        roxoInOut  = roxo ++ reverse roxo

palleteR, palleteG, palleteB :: CArray Int Pixel8
(palleteR, palleteG, palleteB) = ( palVec [fst3 x | x <- pallete],
                                   palVec [snd3 x | x <- pallete],
                                   palVec [thr x | x <- pallete]
                                 )

maxi :: Int
maxi = length laranja*2 + length verde*2 + length azul*2 + length roxo*2


-----------------------------------------------------------
-- Cálculo do Mandelbrot
-----------------------------------------------------------
calcPoint :: Int -> CComplex -> CComplex -> Int -> (Int, Double)
calcPoint iterations (cx,cy) (!zx,!zy) !iter
  | iter < iterations && magnitudeSquared < 16 = calcPoint iterations (cx,cy) nextZ (iter+1)
  | otherwise = (iter, sqrt magnitudeSquared)
  where
    !magnitudeSquared = zx*zx + zy*zy
    !nextZ = (zx*zx - zy*zy + cx, 2*zx*zy + cy)

colorFromIter :: Int -> Int -> (Int, Double) -> PixelRGB8
colorFromIter iterations hue iterPoint
  | iter == iterations = PixelRGB8 0 0 0
  | otherwise = PixelRGB8 (palleteR!i) (palleteG!i) (palleteB!i)
  where (iter,mag) = iterPoint
        i = mod (truncate (color * toFloat maxi)) maxi
        color = toFloat ix / toFloat points -- Entre 0 e 1, representa um ciclo de cores
        ix = truncate (sqrt (toFloat iter + 1 - logBase 2 (logBase 2 mag))*200 + toFloat hue*4 + toFloat hueOffset ) `mod` points
        points = 2048

genIter :: Int -> Int -> Int -> Int -> Int -> (Int, Double)
genIter iterations x y frame db = calcPoint iterations (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size+coordX
        yPos = (y'-h/2)/size+coordY
        (w, h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)
        size = if frame == -1 then 2 ** fixedZoom else zoomFor frame db


-----------------------------------------------------------
-- Funções de matrizes/vetores
-----------------------------------------------------------
cMatrix :: [Int] -> CArray (Int, Int) Int
cMatrix = listArray ((0, 0), (width,height))

cMatrixDouble :: [Double] -> CArray (Int, Int) Double
cMatrixDouble = listArray ((0, 0), (width,height))

matrixIter :: CArray (Int, Int) Int
matrixIter = cMatrix [fst $ genIter maxIter x y (-1) 0 | x <- [0..width], y <- [0..height]]

matrixMag :: CArray (Int, Int) Double
matrixMag = cMatrixDouble [snd $ genIter maxIter x y (-1) 0 | x <- [0..width], y <- [0..height]]

readIter :: Int -> Int -> Int
readIter x y = matrixIter ! (x,y)

readMag :: Int -> Int -> Double
readMag x y = matrixMag ! (x,y)

palVec :: [Pixel8] -> CArray Int Pixel8
palVec = listArray (0,maxi)

cudaPalette :: VS.Vector Pixel8
cudaPalette = VS.fromList [component | (red, green, blue) <- pallete, component <- [red, green, blue]]


-----------------------------------------------------------
-- Funções FFT
-----------------------------------------------------------
-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
doAnim :: ((Int, Int),Int) -> Bool -> IO (Image PixelRGB8)
doAnim info static
  | not static && zoom < deepZoomThreshold = do
      cudaImage <- generateImageCuda iterations zoom hue
      maybe (generateImageParallel genPixel width height) pure cudaImage
  | not static = do
      cudaImage <- generateImageDeepCuda iterations zoom hue (deepFrame width height iterations zoom coordXText coordYText)
      maybe (generateImageParallel genPixel width height) pure cudaImage
  | otherwise = generateImageParallel genPixel width height
  where genPixel x y = colorFromIter iterations hue $ if static then (readIter x y, readMag x y) else renderPoint x y
        -- 'hueDB' is intentionally the accumulated bass that drives zoom.
        -- Do not feed it back into the palette: that made its colour response
        -- appear stronger later in the same (deeper) animation.  Palette
        -- movement now depends only on the current normalized bass and time.
        hue = mod (db `div` (20*sensitivity) + frameN `div` 60) maxi
        (path, frameN, db) = (genPath $ fst s, fromIntegral $ fst s, snd s)
        (s, hueDB) = info
        zoom = zoomFor frameN hueDB
        iterations = if static then maxIter else iterationsFor zoom
        deep = if zoom >= deepZoomThreshold then Just (deepFrame width height iterations zoom coordXText coordYText) else Nothing
        renderPoint x y = maybe (genIter iterations x y frameN hueDB) (\frame -> deepPoint frame x y) deep

deepZoomThreshold :: Double
deepZoomThreshold = 1.0e12

zoomFor :: Int -> Int -> Double
zoomFor frame db = min maxZoom (2 ** zoomExponent frame db)

zoomExponent :: Int -> Int -> Double
zoomExponent frame db = fromIntegral (frame + db `div` 160)/(framerate*10)+7

-- More detail becomes visible as the viewport narrows.  The cap keeps deep
-- zoom usable while still allowing complex boundary regions to converge.
iterationsFor :: Double -> Int
iterationsFor zoom = min 4096 (maxIter + 64 * max 0 (floor (logBase 2 zoom) - 7))

-- | Rendering a row is independent from every other row.  The strategy makes
-- each row fully strict before the image is encoded, allowing the RTS to use
-- every capability requested with @+RTS -N@ while retaining bounded frame
-- memory.
generateImageParallel :: (Int -> Int -> PixelRGB8) -> Int -> Int -> IO (Image PixelRGB8)
generateImageParallel pixel imageWidth imageHeight = do
  pixels <- VSM.unsafeNew (imageWidth * imageHeight * 3)
  nextRow <- newIORef 0
  capabilities <- getNumCapabilities
  completed <- forM [0..min imageHeight capabilities - 1] $ \capability -> do
    done <- newEmptyMVar :: IO (MVar (Either SomeException ()))
    _ <- forkOn capability $ do
      result <- try (renderRows pixels nextRow)
      putMVar done result
    pure done
  results <- mapM takeMVar completed
  mapM_ (either throwIO pure) results
  Image imageWidth imageHeight <$> VS.unsafeFreeze pixels
  where
    renderRows pixels nextRow = do
      row <- atomicModifyIORef' nextRow (\current -> (current + 1, current))
      when (row < imageHeight) $ do
        forM_ [0..imageWidth - 1] $ \column ->
          case pixel column row of
            PixelRGB8 red green blue -> do
              let offset = (row * imageWidth + column) * 3
              VSM.unsafeWrite pixels offset red
              VSM.unsafeWrite pixels (offset + 1) green
              VSM.unsafeWrite pixels (offset + 2) blue
        renderRows pixels nextRow

-- CUDA uses Double for ordinary frames. Deep frames retain the CPU
-- perturbation renderer because their reference orbit uses arbitrary precision.
generateImageCuda :: Int -> Double -> Int -> IO (Maybe (Image PixelRGB8))
generateImageCuda iterations zoom hue = do
  pixels <- VSM.unsafeNew (width * height * 3)
  rendered <- cudaRenderFrame pixels cudaPalette width height iterations zoom coordX coordY hue
  if rendered
    then Just . Image width height <$> VS.unsafeFreeze pixels
    else pure Nothing

generateImageDeepCuda :: Int -> Double -> Int -> DeepFrame -> IO (Maybe (Image PixelRGB8))
generateImageDeepCuda iterations zoom hue frame = do
  pixels <- VSM.unsafeNew (width * height * 3)
  rendered <- cudaRenderDeepFrame pixels cudaPalette (deepReferenceOrbit frame) width height iterations zoom hue
  if rendered
    then Just . Image width height <$> VS.unsafeFreeze pixels
    else pure Nothing

doAnimSave :: ((Int, Int),Int) -> FilePath -> Bool -> IO ()
doAnimSave info path static = do
  image <- doAnim info static
  writePng path image


writeVideo :: [((Int, Int), Int)] -> Int -> IO ()
writeVideo dbList total = mask $ \restore -> do
  -- ffmpeg-light rounded 60 fps to 16 ms PTS steps, producing a 62.5 fps
  -- MKV.  Feeding raw RGB to FFmpeg declares the rate as an exact rational
  -- and keeps audio reactions locked to the encoded video timeline.
  (Just videoInput, _, _, encoder) <- createProcess (proc "ffmpeg"
    [ "-hide_banner", "-loglevel", "error", "-y"
    , "-f", "rawvideo", "-pixel_format", "rgb24"
    , "-video_size", printf "%dx%d" width height
    , "-framerate", show (round framerate)
    , "-i", "pipe:0", "-an", "-c:v", "libx264", "-preset", "medium"
    , "-crf", "20", "-pix_fmt", "yuv420p", "-f", "matroska", videoPath
    ]) { std_in = CreatePipe }
  hSetBuffering videoInput NoBuffering
  let save :: Image PixelRGB8 -> IO ()
      save image = VS.unsafeWith (imageData image) $ \pixelPointer ->
        hPutBuf videoInput pixelPointer (VS.length (imageData image))
  let renderFrames =
        mapM_ (\(n, info) -> do
          status total n
          image <- doAnim info estático
          save image
        ) (zip [1..] dbList)

  interrupted <- (restore renderFrames >> pure False) `catch` \UserInterrupt -> do
    putStrLn "\nInterrupção recebida; finalizando o vídeo parcial..."
    pure True

  -- Keep this masked: a second Ctrl+C cannot interrupt FFmpeg while it writes
  -- the Matroska trailer, which is what makes a partial render playable.
  hClose videoInput
  encoderResult <- waitForProcess encoder
  case encoderResult of
    ExitSuccess -> pure ()
    ExitFailure code -> exitFalha ("FFmpeg terminou com código " ++ show code)
  callCommand $ printf "ffmpeg -i %s -i %s -map 0:v -map 1:a -c:v copy -c:a copy -shortest %s -y" videoPath musicPath "./mandelbrot_audio.mkv"
  when interrupted $ putStrLn "Vídeo parcial finalizado com áudio."


-----------------------------------------------------------
-- Main
-----------------------------------------------------------
main :: IO ()
main = do
  _ <- hSetBuffering stdout NoBuffering

  p <- getWAVEFile musicPath --Lendo nosso arquivo

  let header = waveHeader p

  -- WAVE armazena o PCM signed como unsigned; recentrar remove o componente
  -- DC antes da FFT e deixa a normalização de graves consistente.
  let waveSmpAll = [(fromIntegral (head l) - 32768) / 32768 | l <- waveSamples p]

  let sampleRate = waveFrameRate header  --Numero de samples e.g 44100

  let waveSmp = drop (sampleRate `div` 9) waveSmpAll -- Isso irá colocar nosso vídeo 100ms adiantado, melhor sincronismo

  let samples = fromMaybe 0 $ waveFrames header     -- Total de samples da nossa música

  _ <- if samples == 0 then exitFalha "Falha na leitura de samples do arquivo." else putStrLn "\nFile OK."

  let framesPerSecond = round framerate

  -- Ceiling ensures the image stream covers the fractional final second of
  -- the audio; FFmpeg's -shortest then trims the final frame precisely.
  let frameCount = (samples * framesPerSecond + sampleRate - 1) `div` sampleRate

  -- 'total' is also the final logical frame index.  AudioTiming emits frames
  -- 2..total, therefore this yields exactly frameCount encoded frames.
  let total = frameCount + 1

  let duração = samples `div` waveFrameRate header

  -- Compute every frame's position from its timestamp, not from a truncated
  -- samples-per-frame value.  This avoids a linear A/V drift at 144 fps.
  let sampleOffset frame = (frame * sampleRate + framesPerSecond `div` 2) `div` framesPerSecond + 1

  let rangeFrames = [0..total]

  _ <- putStrLn $ printf "%dx%d - %.f fps" width height framerate
  _ <- putStrLn $ printf "Sample Count: %d" samples
  _ <- putStrLn $ printf "SampleRate: %d\nduracao em segundos: %d\nNumero de frames: %d" sampleRate duração total


  {- Chamamos essa função para retornar uma lista de informações úteis para
     desenharmos o mandelbrot reagindo a música.
     [((a, b), c)] onde a é o numero do frame
     b é quantidade de grave que o fft disse que tem no frame atual
     c é a versão suavizada de b, objetivo dela é não ter picos extremos -}
  let dbList = drop dropped $ getAnimationTimingsAt bassTarget waveSmp rangeFrames sampleOffset duração

  _ <- putStr "Deseja salvar em vídeo? [Y/N] "
  opt <- getLine

  _ <- if null opt || 'y' /= toLower (head opt) && 'n' /= toLower (head opt)
    then exitFalha "Não entendi. Abortando."
    else putStrLn (if 'y' == toLower (head opt) then "Saída de vídeo" else "Saída em imagens")


  if 'n' == toLower (head opt)
    then mapM_ (\(n, x) -> status total n >> doAnimSave x (genPath n) estático) $ zip [dropped..] dbList

    else writeVideo dbList total

  putStrLn "\nDone"
