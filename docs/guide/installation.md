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

FiNeR uses **git submodules** for its third-party dependencies. Clone recursively:

```bash
git clone https://github.com/szaghi/FiNeR --recursive
cd FiNeR
```

If you already have a non-recursive clone:

```bash
git submodule update --init --recursive
```

### Third-Party Dependencies

The submodules live under `src/third_party/`:

| Library | Purpose |
|---------|---------|
| [PENF](https://github.com/szaghi/PENF) | Portable numeric kind parameters (`I4P`, `R8P`, etc.) |
| [StringiFor](https://github.com/szaghi/StringiFor) | `string` type used throughout for string operations |
| [FACE](https://github.com/szaghi/FACE) | ANSI terminal color/style support |
| [FLAP](https://github.com/szaghi/FLAP) | Fortran command-line argument parser |
| [BeFoR64](https://github.com/szaghi/BeFoR64) | Base64 encoding |

## Build with CMake (preferred)

CMake is the recommended build system for library use and integration into other projects.

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

To embed FiNeR in an existing CMake project, place a recursive clone of FiNeR alongside your sources and add to your `CMakeLists.txt`:

```cmake
add_subdirectory(FiNeR)

target_link_libraries(your_target FiNeR::FiNeR)
```

## Build with FoBiS.py

[FoBiS.py](https://github.com/szaghi/FoBiS) is used by the CI pipeline for coverage analysis and documentation generation.

```bash
pip install FoBiS.py
```

### List all build modes

```bash
FoBiS.py build -lmodes
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
FoBiS.py build -mode tests-gnu
./scripts/run_tests.sh
```

Compiled test executables are placed in `./exe/`.

### Build the library

```bash
# Static library (GNU gfortran)
FoBiS.py build -mode finer-static-gnu

# Shared library (GNU gfortran)
FoBiS.py build -mode finer-shared-gnu

# Static library (Intel Fortran)
FoBiS.py build -mode finer-static-intel
```

The library is placed in `./static/` or `./shared/` respectively.

### Coverage and documentation

```bash
FoBiS.py rule -ex makecoverage   # build + run tests + gcov report
FoBiS.py rule -ex makedoc        # build ford API documentation
```
