#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
# Keep the recurrence numerically close to GHC's scalar Double operations.
# Fusing multiply/add is faster, but changes pixels sitting on the boundary.
nvcc -O3 --fmad=false -arch=sm_86 -Xcompiler -fPIC -shared -o libmandelbrot_cuda.so mandelbrot_cuda.cu
