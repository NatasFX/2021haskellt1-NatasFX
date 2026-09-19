Saudações;

Este é meu trabalho 1 da disciplina de Paradigmas de Programação.

[![Não consegui carregar a imagem :(](https://i.imgur.com/7i8F4R7.png)](https://youtu.be/3HhPy7bERLY)

O Programa gera um vídeo de zoom no Conjunto de Mandelbrot, sincronizando com as batidas de uma música fornecida.

Aqui tem um exemplo do que ele faz. https://youtu.be/yOgwc98WI6c https://youtu.be/w8SpVDVj1yM

Existem outras versões deste projeto. A que ficou melhor foi esta. Caso interessar estão nas outras branchs.

Inputs de usuário se encontram dentro do código.

## Progresso após a versão FFT

Além da análise FFT já presente no trabalho, o renderer recebeu uma revisão
completa para permitir vídeos longos, zoom profundo e uso efetivo do hardware
atual.

- A leitura do WAVE agora recentra o PCM antes da FFT e normaliza o grave pelo
  percentil 95%; picos não aceleram o zoom de forma desproporcional.
- O cálculo das janelas de áudio usa vetores e scans incrementais, preservando
  exatamente os timings da implementação original. O teste de caracterização
  compara os dois resultados.
- A imagem no CPU é escrita diretamente em um vetor RGB mutável, com linhas
  distribuídas pelas capabilities do RTS, em vez de construir listas de pixels.
- O número de iterações cresce conforme o zoom. O começo permanece barato e a
  fronteira recebe mais detalhe quando necessário.
- `Ctrl+C` fecha o encoder antes de sair, portanto o vídeo parcial continua
  reproduzível.

## Zoom profundo e precisão

Nos zooms rasos o Mandelbrot é calculado diretamente em `Double`. A partir de
`1e12`, o programa constrói uma órbita de referência em ponto fixo com bits de
guarda e calcula os deltas de cada pixel por *perturbation theory*. Isso evita
que a coordenada central seja arredondada para outro local ao ultrapassar a
precisão de `Double`.

As coordenadas centrais são strings em `src/Main.hs` de propósito: um literal
`Double` já descartaria os dígitos necessários para o deep zoom. O limite
prático é `1e300`, definido pelo alcance de `Double` nos deltas por pixel, não
pela precisão da órbita central.

## CUDA

Os dois caminhos de render usam CUDA em `Float64` quando disponível:

- zoom raso: iteração direta por pixel;
- zoom profundo: a órbita precisa é feita uma vez no CPU e a perturbação de
  todos os pixels é executada na GPU.

A paleta é copiada com o tamanho exato de 2.032 cores, evitando a leitura além
do vetor que produzia bandas espúrias no ciclo de cores. Caso CUDA retorne um
erro, há fallback para o renderer paralelo de CPU.

No WSL usado no desenvolvimento (RTX 3070, `sm_86`), um frame profundo de
`1920x1080`, zoom `1e12` e 1.088 iterações caiu de aproximadamente 1,5 s no
CPU para aproximadamente 0,45 s na primeira execução CUDA. O valor varia com
a GPU e o driver.

### Executar

Pré-requisitos: GHC/Cabal compatíveis com o arquivo `.cabal`, FFTW (`libfftw3-dev`),
FFmpeg, driver NVIDIA exposto no WSL e CUDA Toolkit (`nvcc`). A arquitetura
CUDA pode ser ajustada em `cuda/build.sh`.

```bash
./render.sh
```

O script compila a biblioteca CUDA, executa o Cabal e inicia o programa com
`+RTS -N -A32m`, usando todas as CPUs expostas pelo WSL no fallback de CPU.

Durante a renderização, `mandelbrot.mkv` pode ser aberto no VLC/mpv mesmo ainda
estando em crescimento. Ao finalizar, o programa gera
`mandelbrot_audio.mkv`, com vídeo e áudio unidos.

### Verificação

Com `mandeloso.wav` na raiz do repositório:

```bash
cabal exec -- runghc -isrc test/DeepZoomSpec.hs
```

O teste cobre os invariantes de precisão profunda e confirma que os timings de
áudio otimizados continuam equivalentes aos da versão FFT original.

  
