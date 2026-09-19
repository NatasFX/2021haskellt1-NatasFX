#!/usr/bin/env bash
set -euo pipefail

project_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$project_dir"
./cuda/build.sh
export LD_LIBRARY_PATH="$project_dir/cuda${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

if [[ ! -f mandeloso.wav ]]; then
  printf 'Arquivo ausente: %s/mandeloso.wav\n' "$project_dir" >&2
  exit 1
fi

mkdir -p anim

# -N escolhe todas as CPUs que o WSL expõe ao processo.  -A32m reduz a pressão
# do garbage collector quando as 16 capabilities renderizam linhas em paralelo.
cabal build
exec cabal run Natã-T1-PP -- +RTS -N -A32m -RTS
