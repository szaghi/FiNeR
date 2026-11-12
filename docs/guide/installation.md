---
title: Installation
---

# Installation

## Prerequisites

A Fortran 2003+ compiler. FiNeR is tested with:

| Compiler | Versions | Where |
|----------|----------|-------|
| GNU gfortran | 16.2 | CI (tests and documentation examples); development: FoBiS, CMake and fpm builds |
| NVIDIA nvfortran | 26.9 | development: CMake build |

::: warning gfortran 14.3 or newer
gfortran 13.3 and 14.2 (the versions tested) miscompile the elements of a `class(*)` character array: `add` with a character array value
writes wrong values (`['p1','p2','p3']` is saved as `p1 1p p2`), with no error. The bug is in the compiler, fixed in
gfortran 14.3; FiNeR does not work around it. Ubuntu 24.04 ships gfortran 13.3 as `gfortran` and 14.2 as `gfortran-14`:
use a newer compiler.
:::

The fobos file also has modes for the Intel compilers (`tests-intel`, `finer-static-intel`, ...), not tested recently.
FiNeR is developed on GNU/Linux.

## Download

Clone the repository:

```bash
git clone https://github.com/szaghi/FiNeR
cd FiNeR
```

FiNeR does **not** use git submodules: a recursive clone fetches nothing more. The third-party dependencies are fetched into `src/third_party/` by [FoBiS.py](https://github.com/szaghi/FoBiS), and they must be there before building with CMake or FoBiS.py (fpm fetches its own, see [Build with fpm](#build-with-fpm)):

```bash
pip install FoBiS.py
fobis fetch --no-build   # clone the dependencies into src/third_party/
```

Without FoBiS.py, clone them by hand:

```bash
for dep in BeFoR64 FACE FLAP PENF StringiFor; do
  git clone https://github.com/szaghi/$dep src/third_party/$dep
done
```

### Third-Party Dependencies

The dependencies live under `src/third_party/`:

| Library | Purpose |
|---------|---------|
| [PENF](https://github.com/szaghi/PENF) | Portable numeric kind parameters (`I4P`, `R8P`, etc.) |
| [StringiFor](https://github.com/szaghi/StringiFor) | `string` type used throughout for string operations |
| [FACE](https://github.com/szaghi/FACE) | ANSI terminal color/style support |
| [FLAP](https://github.com/szaghi/FLAP) | Fortran command-line argument parser |
| [BeFoR64](https://github.com/szaghi/BeFoR64) | Base64 encoding |

## Build with CMake (preferred)

CMake is the recommended build system for library use and integration into other projects. It builds the dependencies from `src/third_party/`, so fetch them first (see [Download](#download)).

```bash
mkdir build && cd build
cmake ..
make
```

### Run the test suite

```bash
ctest          # run all tests
ctest -R <test_name>   # run a single named test
ctest -V       # verbose output
```

Each test prints `"Are all tests passed? T"` on success.

### CMake subdirectory integration

To embed FiNeR in an existing CMake project, place a clone of FiNeR alongside your sources, fetch its dependencies into `FiNeR/src/third_party/` (see [Download](#download)), and add to your `CMakeLists.txt`:

```cmake
add_subdirectory(FiNeR)

target_link_libraries(your_target FiNeR::FiNeR)
```

`FetchContent` alone is not enough, because it downloads FiNeR without its dependencies: the configure step fails on the missing `src/third_party/` directories.

## Build with fpm

[fpm](https://fpm.fortran-lang.org) fetches the dependencies itself from `fpm.toml` (PENF and StringiFor, which brings
BeFoR64 and FACE), so `src/third_party/` is not needed. To use FiNeR in an fpm project, add to its `fpm.toml`:

```toml
[dependencies]
FiNeR = { git = "https://github.com/szaghi/FiNeR" }   # or pin a release: tag = "vX.Y.Z"
```

To build and test FiNeR itself:

```bash
fpm build
fpm test                       # run all tests
fpm test finer_test_get        # run a single test
```

`fpm test` runs the tests from the project root, where they find their data files (`src/tests/*.ini`).

::: warning git `color.diff = always`
fpm reads the commit of each dependency from the `git log` header. With `color.diff = always` (or `color.ui = always`)
in your git configuration the commit carries an ANSI escape, and the next fpm run fails parsing `build/cache.toml`
("unexpected invalid sequence"). Use `auto`, or run fpm with
`GIT_CONFIG_PARAMETERS="'color.ui=auto' 'color.diff=auto'"`, then remove `build/`.
:::

## Build with FoBiS.py

[FoBiS.py](https://github.com/szaghi/FoBiS) is used by the CI pipeline for coverage analysis and documentation generation.

```bash
pip install FoBiS.py
```

### List all build modes

```bash
fobis build --lmodes
```

Available modes:

| Mode | Description |
|------|-------------|
| `tests-gnu` | Build all tests with gfortran (release) |
| `tests-gnu-debug` | Build all tests with gfortran (debug) |
| `tests-intel` | Build all tests with ifort (release) |
| `tests-intel-debug` | Build all tests with ifort (debug) |
| `finer-static-gnu` | Static library with gfortran |
| `finer-shared-gnu` | Shared library with gfortran |
| `finer-static-intel` | Static library with ifort |
| `finer-shared-intel` | Shared library with ifort |

### Build and run tests

```bash
fobis fetch
fobis build --mode tests-gnu
./scripts/run_tests.sh
```

Compiled test executables are placed in `./exe/`.

### Build the library

```bash
# Static library (GNU gfortran)
fobis build --mode finer-static-gnu

# Shared library (GNU gfortran)
fobis build --mode finer-shared-gnu

# Static library (Intel Fortran)
fobis build --mode finer-static-intel
```

The library is placed in `./static/` or `./shared/` respectively, with its module files (and those of its dependencies)
in `mod/` under the same directory. To compile a program against the static library:

```bash
gfortran -Istatic/mod my_program.f90 static/libfiner.a -o my_program
```

### Quadruple precision

`R16P` reals and complex numbers are quadruple precision only when PENF is compiled with the macro `PENF_R16P`: the
FoBiS modes define it, CMake defines it when the compiler supports a quadruple precision real, fpm does not (there
`R16P` is the kind of `R8P`, and `get` and `add` of `R16P` values work in double precision).

### Coverage and documentation

```bash
fobis rule --ex makecoverage   # build + run tests + gcov report
fobis rule --ex makedoc        # build API documentation
bash scripts/docs_examples.sh  # build and run the documentation examples, regenerate their outputs
cd docs && npm run docs:dev    # the documentation site, locally
```
