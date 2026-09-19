module CudaRender (cudaRenderFrame, cudaRenderDeepFrame) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "mandelbrot_cuda_render"
  c_mandelbrot_cuda_render :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CInt -> IO CInt

foreign import ccall unsafe "mandelbrot_cuda_render_deep"
  c_mandelbrot_cuda_render_deep :: Ptr Word8 -> Ptr Word8 -> Ptr CDouble -> CInt -> CInt -> CInt -> CDouble -> CInt -> IO CInt

cudaRenderFrame :: VSM.IOVector Word8 -> VS.Vector Word8 -> Int -> Int -> Int -> Double -> Double -> Double -> Int -> IO Bool
cudaRenderFrame pixels palette width height iterations zoom centerX centerY hue =
  VSM.unsafeWith pixels $ \pixelPointer -> VS.unsafeWith palette $ \palettePointer -> do
    ok <- c_mandelbrot_cuda_render pixelPointer palettePointer (fromIntegral width) (fromIntegral height)
            (fromIntegral iterations) (realToFrac zoom) (realToFrac centerX)
            (realToFrac centerY) (fromIntegral hue)
    pure (ok /= 0)

cudaRenderDeepFrame :: VSM.IOVector Word8 -> VS.Vector Word8 -> VS.Vector CDouble -> Int -> Int -> Int -> Double -> Int -> IO Bool
cudaRenderDeepFrame pixels palette orbit width height iterations zoom hue =
  VSM.unsafeWith pixels $ \pixelPointer -> VS.unsafeWith palette $ \palettePointer -> VS.unsafeWith orbit $ \orbitPointer -> do
    ok <- c_mandelbrot_cuda_render_deep pixelPointer palettePointer orbitPointer
            (fromIntegral width) (fromIntegral height) (fromIntegral iterations)
            (realToFrac zoom) (fromIntegral hue)
    pure (ok /= 0)
