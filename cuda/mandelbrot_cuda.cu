#include <cuda_runtime.h>
#include <cmath>

namespace {
// Each base ramp has 254 samples and is mirrored: 4 * 254 * 2.
// Keep this in lockstep with Main.pallete; 2040 read past the Haskell vector.
constexpr int paletteLength = 2032;

__device__ void writeColour(unsigned char *pixels, const unsigned char *palette,
                            int output, int iter, int iterations, double mag2,
                            double zoom, int hue) {
  if (iter == iterations) {
    pixels[output] = pixels[output + 1] = pixels[output + 2] = 0;
    return;
  }
  const double logTwo = log(2.0);
  const double magnitude = sqrt(mag2);
  const double inner = log(magnitude) / logTwo;
  // Match the CPU's zoom-compensated smooth colouring.  The initial frame is
  // 2^7, so its palette phase is unchanged.
  const double smoothIter = double(iter) + 1.0 - log(inner) / logTwo;
  const double zoomColourOffset = fmax(0.0, log(zoom) / logTwo - 7.0);
  const int rawPhase = int(sqrt(fmax(0.0, smoothIter - zoomColourOffset)) * 200.0
                           + double(hue) * 4.0 - 150.0);
  int phase = rawPhase % 2048;
  if (phase < 0) phase += 2048;
  const int paletteIndex = (phase * paletteLength) / 2048;
  pixels[output] = palette[paletteIndex * 3];
  pixels[output + 1] = palette[paletteIndex * 3 + 1];
  pixels[output + 2] = palette[paletteIndex * 3 + 2];
}

__global__ void renderKernel(unsigned char *pixels, const unsigned char *palette,
                             int width, int height, int iterations, double zoom,
                             double centerX, double centerY, int hue) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  const int y = blockIdx.y * blockDim.y + threadIdx.y;
  if (x >= width || y >= height) return;
  const double cx = (double(x) - double(width) / 2.0) / zoom + centerX;
  const double cy = (double(y) - double(height) / 2.0) / zoom + centerY;
  double zx = 0.0, zy = 0.0, mag2 = 0.0;
  int iter = 0;
  while (iter < iterations && mag2 < 16.0) {
    const double nx = zx * zx - zy * zy + cx;
    zy = 2.0 * zx * zy + cy;
    zx = nx;
    mag2 = zx * zx + zy * zy;
    ++iter;
  }
  const int output = (y * width + x) * 3;
  writeColour(pixels, palette, output, iter, iterations, mag2, zoom, hue);
}

__global__ void renderDeepKernel(unsigned char *pixels, const unsigned char *palette,
                                 const double *orbit, int width, int height,
                                 int iterations, double zoom, int hue) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  const int y = blockIdx.y * blockDim.y + threadIdx.y;
  if (x >= width || y >= height) return;
  const double dcReal = (double(x) - double(width) / 2.0) / zoom;
  const double dcImaginary = (double(y) - double(height) / 2.0) / zoom;
  double deltaReal = 0.0, deltaImaginary = 0.0, mag2 = 0.0;
  int iter = 0;
  while (iter < iterations) {
    const double realHi = orbit[iter * 4];
    const double realLo = orbit[iter * 4 + 1];
    const double imaginaryHi = orbit[iter * 4 + 2];
    const double imaginaryLo = orbit[iter * 4 + 3];
    const double linearReal = (realHi * deltaReal - imaginaryHi * deltaImaginary)
                            + (realLo * deltaReal - imaginaryLo * deltaImaginary);
    const double linearImaginary = (realHi * deltaImaginary + imaginaryHi * deltaReal)
                                 + (realLo * deltaImaginary + imaginaryLo * deltaReal);
    const double nextReal = 2.0 * linearReal + deltaReal * deltaReal - deltaImaginary * deltaImaginary + dcReal;
    const double nextImaginary = 2.0 * linearImaginary + 2.0 * deltaReal * deltaImaginary + dcImaginary;
    deltaReal = nextReal;
    deltaImaginary = nextImaginary;
    const double nextRealHi = orbit[(iter + 1) * 4];
    const double nextRealLo = orbit[(iter + 1) * 4 + 1];
    const double nextImaginaryHi = orbit[(iter + 1) * 4 + 2];
    const double nextImaginaryLo = orbit[(iter + 1) * 4 + 3];
    const double pointReal = nextRealHi + nextRealLo + deltaReal;
    const double pointImaginary = nextImaginaryHi + nextImaginaryLo + deltaImaginary;
    mag2 = pointReal * pointReal + pointImaginary * pointImaginary;
    ++iter;
    if (mag2 >= 16.0 || !isfinite(mag2)) break;
  }
  writeColour(pixels, palette, (y * width + x) * 3, iter, iterations, mag2, zoom, hue);
}
}

extern "C" int mandelbrot_cuda_render(unsigned char *hostPixels,
                                      const unsigned char *hostPalette,
                                      int width, int height, int iterations,
                                      double zoom, double centerX,
                                      double centerY, int hue) {
  static unsigned char *devicePixels = nullptr;
  static unsigned char *devicePalette = nullptr;
  static size_t pixelCapacity = 0;
  const size_t pixels = size_t(width) * height;
  if (pixels > pixelCapacity) {
    if (devicePixels) cudaFree(devicePixels);
    if (cudaMalloc(&devicePixels, pixels * 3) != cudaSuccess) return 0;
    pixelCapacity = pixels;
  }
  if (!devicePalette && cudaMalloc(&devicePalette, paletteLength * 3) != cudaSuccess) return 0;
  if (cudaMemcpy(devicePalette, hostPalette, paletteLength * 3, cudaMemcpyHostToDevice) != cudaSuccess) return 0;
  dim3 block(16, 16);
  dim3 grid((width + block.x - 1) / block.x, (height + block.y - 1) / block.y);
  renderKernel<<<grid, block>>>(devicePixels, devicePalette, width, height,
                                iterations, zoom, centerX, centerY, hue);
  if (cudaGetLastError() != cudaSuccess) return 0;
  return cudaMemcpy(hostPixels, devicePixels, pixels * 3, cudaMemcpyDeviceToHost) == cudaSuccess;
}

extern "C" int mandelbrot_cuda_render_deep(unsigned char *hostPixels,
                                           const unsigned char *hostPalette,
                                           const double *hostOrbit, int width,
                                           int height, int iterations,
                                           double zoom, int hue) {
  static unsigned char *devicePixels = nullptr;
  static unsigned char *devicePalette = nullptr;
  static double *deviceOrbit = nullptr;
  static size_t pixelCapacity = 0, orbitCapacity = 0;
  const size_t pixelCount = size_t(width) * height;
  const size_t orbitCount = size_t(iterations + 1) * 4;
  if (pixelCount > pixelCapacity) {
    if (devicePixels) cudaFree(devicePixels);
    if (cudaMalloc(&devicePixels, pixelCount * 3) != cudaSuccess) return 0;
    pixelCapacity = pixelCount;
  }
  if (!devicePalette && cudaMalloc(&devicePalette, paletteLength * 3) != cudaSuccess) return 0;
  if (orbitCount > orbitCapacity) {
    if (deviceOrbit) cudaFree(deviceOrbit);
    if (cudaMalloc(&deviceOrbit, orbitCount * sizeof(double)) != cudaSuccess) return 0;
    orbitCapacity = orbitCount;
  }
  if (cudaMemcpy(devicePalette, hostPalette, paletteLength * 3, cudaMemcpyHostToDevice) != cudaSuccess ||
      cudaMemcpy(deviceOrbit, hostOrbit, orbitCount * sizeof(double), cudaMemcpyHostToDevice) != cudaSuccess) return 0;
  dim3 block(16, 16);
  dim3 grid((width + block.x - 1) / block.x, (height + block.y - 1) / block.y);
  renderDeepKernel<<<grid, block>>>(devicePixels, devicePalette, deviceOrbit,
                                    width, height, iterations, zoom, hue);
  if (cudaGetLastError() != cudaSuccess) return 0;
  return cudaMemcpy(hostPixels, devicePixels, pixelCount * 3, cudaMemcpyDeviceToHost) == cudaSuccess;
}
