# FiNeR

**Fortran INI ParseR and generator** — a pure Fortran 2003+ OOP library for reading and writing [INI](https://en.wikipedia.org/wiki/INI_file) configuration files.

[![CI](https://github.com/szaghi/FiNeR/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/FiNeR/actions)
[![Coverage](https://img.shields.io/codecov/c/github/szaghi/FiNeR.svg)](https://app.codecov.io/gh/szaghi/FiNeR)
[![GitHub tag](https://img.shields.io/github/tag/szaghi/FiNeR.svg)](https://github.com/szaghi/FiNeR/releases)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

---

## Features

- Parse INI files from disk or from an in-memory string
- Auto-detect all sections and options — no schema required
- Multi-line option values and inline comments (`;`) handled automatically
- Configurable option separator (default `=`)
- Generate INI files programmatically with `add`/`del`/`save`
- Unlimited polymorphic values: integer, real, logical, character, and arrays of any PENF kind
- Introspection: `has_section`, `has_option`, `index`, `count_values`, `items`, `loop`
- OOP/TDD designed — single `file_ini` type, all functionality as type-bound procedures

**[Documentation](https://szaghi.github.io/FiNeR/)** | **[API Reference](https://szaghi.github.io/FiNeR/api/)**

---

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](https://szaghi.github.io/FiNeR/guide/contributing) page.

## Copyrights

This project is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

> Anyone interested in using, developing, or contributing to this project is welcome — pick the license that best fits your needs.

---

## Quick start

Parse an INI source and retrieve a multi-value option:

```fortran
use finer
use penf, only: R4P, I4P
implicit none
type(file_ini)                :: fini
character(len=:), allocatable :: source
real(R4P), allocatable        :: array(:)
integer(I4P)                  :: error

source = '[section-1]'//new_line('A')// &
         'option-1 = one'//new_line('A')// &
         'option-2 = 2.'//new_line('A')// &
         '           3.'//new_line('A')// &
         '[section-2]'//new_line('A')// &
         'option-1 = foo'

call fini%load(source=source)
allocate(array(1:fini%count_values(section_name='section-1', option_name='option-2')))
call fini%get(section_name='section-1', option_name='option-2', val=array, error=error)
if (error == 0) print *, array   ! 2.0  3.0
```

Generate an INI file:

```fortran
use finer
use penf, only: R8P
implicit none
type(file_ini) :: fini

call fini%add(section='sec-foo')
call fini%add(section='sec-foo', option='bar',   val=-32.1_R8P)
call fini%add(section='sec-foo', option='baz',   val=' hello FiNeR! ')
call fini%add(section='sec-foo', option='array', val=[1, 2, 3, 4])
call fini%add(section='sec-bar')
call fini%add(section='sec-bar', option='bools', val=[.true., .false., .false.])
call fini%save(filename='foo.ini')
```

---

## Install

### Clone and build with CMake

```sh
git clone https://github.com/szaghi/FiNeR --recursive
cd FiNeR
mkdir build && cd build
cmake ..
make && ctest
```

### CMake subdirectory integration

```cmake
add_subdirectory(FiNeR)
target_link_libraries(your_target FiNeR::FiNeR)
```

| Tool | Command |
|------|---------|
| CMake | `cmake .. && make` |
| FoBiS.py | `FoBiS.py build -mode tests-gnu` |
