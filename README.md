# noise
Noise generators written in Common Lisp

## Requirements
- SBCL
- sh compatible shell

## Cloning, building, and installing

Clone and navigate to the repository:

```console
$ git clone https://github.com/jakergrossman/lnoise.git
$ cd lnoise
```

Build with specified BUILD_DIR (default `bin'):

```console
$ BUILD_DIR="/path/to/build/location" ./build.sh  # build binaries in `foo'
```

Install with specified PREFIX (default `/usr/local/bin'):

```console
$ PREFIX="/path/to/install/location" ./install.sh
```

To build and install in one step:

```console
$ BUILD_DIR=bin PREFIX="/usr/local/bin" ./build.sh && ./install.sh
```

## Running

### lnoise
Prints data points of uniform noise to `stdout`.
```
Usage: lnoise OPTIONS
Available options:
  -h, --help     Print this help text
  -s, --size ARG The size of the noise data
```

### lperlin
Prints data points of perlin noise to `stdout`.

```
Usage: lperlin OPTIONS
Available options:
  -h, --help        Print this help text
  -s, --size ARG    The size of the noise data
  --octaves ARG     Number of octaves to use for perlin noise
  --persistence ARG Weight of each octave relative to the last
```

### lnoise-png
Generates a PNG of uniform noise.
```
Usage: lnoise-png OPTIONS
Available options:
  -h, --help           Print this help text
  -s, --size ARG       The side length of the generated image
  -o, --output-file    Name of the output PNG file [Default: output.png]
  -c, --color-type ARG The color type to use
                       One of [`GRAYSCALE', `TRUECOLOR'] [Default: GRAYSCALE]
```

### lperlin-png
Generates a PNG of perlin noise.
```
Usage: lperlin-png OPTIONS
Available options:
  -h, --help           Print this help text
  -s, --size ARG       The side length of the generated image
  -o, --output-file    Name of the output PNG file [Default: output.png]
  -c, --color-type ARG The color type to use
                       One of [`GRAYSCALE', `TRUECOLOR'] [Default: GRAYSCALE]
  --octaves ARG        Number of octaves to use for perlin noise
  --persistence ARG    Weight of each octave relative to the last
```


## Examples

- 8 data points of uniform noise:

```console
$ lnoise -s 8
-0.5210538 -0.17991185 0.52184796 -0.70521855 -0.8276899 0.998374 -0.46957827 -0.14108133
```

- 6x6 data point grid of perlin noise:

```console
$ lperlin -s 6 | column -t
-0.1583092    0.17558299   -0.3338921    0.15830912   0.093278445  0.119155265
0.0           0.0          -0.09327855   0.2688612    0.0          -0.09327855
0.12782596    -0.26886156  -0.08460764   0.065098226  0.17558302   -0.17788617
-0.1951599    0.2688612    -0.019577026  -0.15600614  0.17558293   0.28613517
-0.093278445  0.0          0.09327855    0.082304634  0.0          0.26886156
0.29704133    0.08230448   0.13873227    -0.10418474  -0.26886156  0.082304426
```

- 256x256 uniform truecolor to "image.png":

```console
$ lnoise-png -s 256 -c truecolor -o "image.png"
```

![1024x1024 grayscale uniform noise](media/example1.png)

- 256x256 perlin grayscale, 3 octaves to "image.png":

```console
$ lperlin-png -s 256 --octaves 3 -o "image.png"
```

![512x512 truecolor perlin noise, 2 octaves](media/example2.png)
