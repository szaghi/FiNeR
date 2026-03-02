# FiNeR

>#### Fortran INI ParseR and generator
>a pure Fortran 2003+ OOP library for reading and writing [INI](https://en.wikipedia.org/wiki/INI_file) configuration files.

[![GitHub tag](https://img.shields.io/github/v/tag/szaghi/FiNeR)](https://github.com/szaghi/FiNeR/tags)
[![GitHub issues](https://img.shields.io/github/issues/szaghi/FiNeR)](https://github.com/szaghi/FiNeR/issues)
[![CI](https://github.com/szaghi/FiNeR/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/FiNeR/actions/workflows/ci.yml)
[![Coverage](https://img.shields.io/codecov/c/github/szaghi/FiNeR.svg)](https://app.codecov.io/gh/szaghi/FiNeR)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

| 📂 **File & string parsing**<br>Load from disk or an in-memory string — no schema required | 💬 **Multi-line & comments**<br>Continuation lines and `;` inline comments handled automatically | 🔤 **Polymorphic values**<br>integer, real, logical, character, and arrays of any PENF kind | ✏️ **Generate INI**<br>Build and save INI files programmatically with `add`/`del`/`save` |
|:---:|:---:|:---:|:---:|
| 🔍 **Introspection**<br>`has_section`, `has_option`, `index`, `count_values`, `items`, `loop` | ⚙️ **Configurable**<br>Custom option separator, comment chars, and inline delimiters | 🏗️ **OOP designed**<br>Single `file_ini` type, all functionality as type-bound procedures | 📦 **Multiple build systems**<br>FoBiS, CMake |

>#### [Documentation](https://szaghi.github.io/FiNeR/)
> For full documentation (guide, API reference, examples, etc...) see the [FiNeR website](https://szaghi.github.io/FiNeR/).

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

```fortran
use finer
use penf, only: R4P
implicit none
type(file_ini)                :: fini
character(len=:), allocatable :: source
real(R4P), allocatable        :: array(:)
integer                       :: error

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

call fini%add(section='sec-foo', option='bar', val=-32.1_R4P)
call fini%save(filename='foo.ini')
```

See [`src/tests/`](src/tests/) for more examples including multi-value arrays, logical options, and file round-trips.

---

## Install

### FoBiS

**Standalone** — clone, fetch dependencies, and build:

```bash
git clone https://github.com/szaghi/FiNeR && cd FiNeR
FoBiS.py fetch                          # fetch BeFoR64, FACE, FLAP, PENF, StringiFor
FoBiS.py build -mode finer-static-gnu   # build static library
```

**As a project dependency** — declare FiNeR in your `fobos` and run `fetch`:

```ini
[dependencies]
deps_dir = src/third_party
FiNeR = https://github.com/szaghi/FiNeR
```

```bash
FoBiS.py fetch           # fetch and build
FoBiS.py fetch --update  # re-fetch and rebuild
```

### CMake

```bash
git clone https://github.com/szaghi/FiNeR --recursive && cd FiNeR
cmake -B build && cmake --build build && ctest --test-dir build
```

**As a CMake subdirectory:**

```cmake
add_subdirectory(FiNeR)
target_link_libraries(your_target FiNeR::FiNeR)
```
