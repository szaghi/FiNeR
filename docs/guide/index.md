---
title: Introduction
---

# Introduction

**FiNeR** (Fortran INI ParseR and generator) reads and writes [INI](https://en.wikipedia.org/wiki/INI_file) files from
modern Fortran: configuration files made of `[sections]` holding `name = value` options. One type, `file_ini`, loads a
file or a string, converts its values into integers, reals, complex numbers, logicals and strings (scalars or lists),
edits them, and saves the result.

```fortran
use finer
use penf, only : I4P, R8P
type(file_ini) :: case_file
real(R8P)      :: dt
integer(I4P)   :: error

call case_file%load(filename='heat.ini', error=error)
call case_file%get(section_name='time', option_name='dt', val=dt, error=error, default=1.0e-4_R8P)
call case_file%add(section_name='time', option_name='dt', val=dt/2)
call case_file%save(filename='restart.ini')
```

## Where to go

| If you want to | Read |
|---|---|
| install it, with FoBiS, CMake or fpm | [Installation](./installation) |
| learn it, one complete program at a time | [the tutorial](/manual/) |
| solve one task now | [the cookbook](/manual/cookbook) |
| know exactly what a method does | [the reference](./features#feature-map), from the [feature map](./features) to the [methods summary](./api-reference) |
| know exactly what an INI file may hold | [The INI syntax](./syntax) |

Every code sample of this documentation is a complete program, built and run by `scripts/docs_examples.sh`; every output
shown is its real output, checked on every push.

## Authors and licenses

FiNeR is written by Stefano Zaghi ([@szaghi](https://github.com/szaghi)); contributions are welcome, see
[Contributing](./contributing). It is distributed under the GPL v3 for free and open source projects, and under the BSD
2-Clause, BSD 3-Clause or MIT license for any other use: pick the one that fits.
