---
title: Installation
---

# Installation

## Prerequisites

A Fortran 2003+ compliant compiler is required. The following compilers are known to work:

| Compiler | Minimum version |
|----------|----------------|
| GNU gfortran | ≥ 5.3.0 |
| Intel Fortran (ifort / ifx) | ≥ 16.x |

FiNeR is developed on GNU/Linux. Windows should work out of the box but is not officially tested.

## Download

Clone the repository:

```bash
git clone https://github.com/szaghi/FiNeR
cd FiNeR
```

FiNeR does **not** use git submodules: a recursive clone fetches nothing more. The third-party dependencies are fetched into `src/third_party/` by [FoBiS.py](https://github.com/szaghi/FoBiS), and they must be there before building with either build system:

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

The library is placed in `./static/` or `./shared/` respectively.

### Coverage and documentation

```bash
fobis rule --ex makecoverage   # build + run tests + gcov report
fobis rule --ex makedoc        # build API documentation
```
