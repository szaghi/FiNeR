# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is FiNeR?

FiNeR (Fortran INI ParseR and generator) is a pure Fortran 2003+ OOP library for reading and writing INI configuration files. It uses `file_ini` as the main user-facing type, backed by a hierarchy of `section` and `option` types.

## Build Systems

FiNeR supports two build systems:

### CMake (preferred for library use)
```bash
mkdir build && cd build
cmake ..
make
ctest          # run all tests
ctest -R <test_name>  # run a single test
```

### FoBiS.py (used by CI for coverage/docs)
```bash
FoBiS.py build -mode tests-gnu          # build tests with gfortran
FoBiS.py build -mode tests-gnu-debug    # debug build
FoBiS.py build -mode finer-static-gnu   # build static library
FoBiS.py rule -ex makecoverage          # build + run tests + gcov
FoBiS.py rule -ex makedoc              # build ford documentation
```

After building tests with FoBiS.py, run them via:
```bash
./scripts/run_tests.sh   # runs all executables in ./exe/
```

## Code Architecture

The library exposes a single module `finer` (re-exporting from internal modules):

```
finer.f90               -- public API re-export module
finer_backend.f90       -- global constants: error codes, comment chars, separators
finer_option_t.F90      -- option type: one key=value pair
finer_section_t.f90     -- section type: named [section] containing option(:)
finer_file_ini_t.F90    -- file_ini type: top-level handler with sections(:)
```

**Dependency chain**: `finer_backend` → `finer_option_t` → `finer_section_t` → `finer_file_ini_t` → `finer`

**Third-party submodules** (in `src/third_party/`):
- **PENF** — portable numeric format kinds (`I4P`, `R8P`, etc.)
- **StringiFor** — `string` type used throughout for string operations
- **FACE** — Fortran ANSI color escape codes
- **FLAP** — Fortran command-line argument parser
- **BeFoR64** — base64 encoding

## INI Format Details

- Comments: lines starting with `!`, `;`, or `#`
- Inline comments: delimited by `;`
- Default option separator: `=`
- Multi-line option values: continuation lines have no `=` (they get joined to the previous option)
- Sections: `[section-name]`

## Error Codes (from `finer_backend`)

| Constant | Value | Meaning |
|---|---|---|
| `ERR_OPTION_NAME` | 1 | bad option name |
| `ERR_OPTION_VALS` | 2 | bad option values |
| `ERR_OPTION` | 3 | generic option error |
| `ERR_SECTION_NAME` | 4 | bad section name |
| `ERR_SECTION_OPTIONS` | 5 | bad section options |
| `ERR_SECTION` | 6 | generic section error |
| `ERR_SOURCE_MISSING` | 7 | no source provided to load |

## Key API (`file_ini` type)

```fortran
use finer, only: file_ini
type(file_ini) :: fini

call fini%load(filename='foo.ini')        ! load from file
call fini%load(source=string_source)      ! load from string
call fini%get(section_name=, option_name=, val=, error=)
call fini%add(section=)                   ! add section
call fini%add(section=, option=, val=)    ! add/update option
call fini%save(filename=)                 ! save to file
call fini%print(unit=)                    ! pretty-print
call fini%has_section(section_name=)      ! logical inquiry
call fini%has_option(section_name=, option_name=)
```

`val` in `get`/`add` is unlimited polymorphic — pass any intrinsic type. Array overloads accept `val(:)` with optional `delimiter`.

## Tests

Test sources are in `src/tests/`. Each test prints `"Are all tests passed? T"` on success. The CMake test system registers each `finer_test_*.f90` as a separate CTest.

The `.F90` extension (uppercase) indicates files that use C preprocessor directives (e.g., `#ifndef __GFORTRAN__` in `finer_file_ini_t.F90` to work around gfortran/ifort differences with `stringifor`).
