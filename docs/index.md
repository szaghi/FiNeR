---
layout: home

hero:
  name: FiNeR
  text: Fortran INI ParseR
  tagline: A pure Fortran 2003+ OOP library for reading and writing INI configuration files.
  actions:
    - theme: brand
      text: Guide
      link: /guide/
    - theme: alt
      text: API Reference
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/FiNeR

features:
  - icon: 📄
    title: INI Parsing
    details: Load INI files from disk or from an in-memory string. Sections and options are auto-detected — no schema required.
  - icon: ✏️
    title: INI Generation
    details: Build INI files programmatically with add/del methods. Save to disk or print to any Fortran unit.
  - icon: 🔍
    title: Introspection
    details: Query the presence of sections and options by name, loop over all options, count multi-value entries, and get section/option indices.
  - icon: 🛠️
    title: Multi Build System
    details: Build with CMake (preferred) or FoBiS.py. Integrate into your CMake project via add_subdirectory and target_link_libraries.
  - icon: 🧪
    title: OOP / TDD Designed
    details: A single file_ini type exposes all functionality as type-bound procedures. Designed with a test-driven approach — each feature is covered by automated tests.
  - icon: 🆓
    title: Free & Open Source
    details: Multi-licensed — GPLv3 for FOSS projects, BSD 2/3-Clause or MIT for commercial use. Fortran 2003+ standard compliant.
---

## Quick start

Parse an INI source string and retrieve a multi-value option:

```fortran
use finer
use penf, only: R4P, I4P
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
allocate(array(1:fini%count_values(section='section-1', option='option-2')))
call fini%get(section='section-1', option='option-2', val=array, error=error)
if (error == 0) print *, array   ! 2.0  3.0
```

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.

## Copyrights

FiNeR is distributed under a multi-licensing system:

| Use case | License |
|----------|---------|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |

> Anyone interested in using, developing, or contributing to FiNeR is welcome — pick the license that best fits your needs.
