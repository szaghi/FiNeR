---
title: Features
---

# Features

## One module, one type

Everything comes from the `finer` module: the `file_ini` type and the error codes. The kinds (`I1P` ... `I8P`, `R4P`,
`R8P`, `R16P`) come from [PENF](https://github.com/szaghi/PENF), which FiNeR depends on.

```fortran
use finer                        ! file_ini and the error codes (ERR_OPTION, ...)
use penf, only : I4P, R8P
type(file_ini) :: fini

call fini%load(filename='heat.ini')
call fini%get(section_name='time', option_name='dt', val=dt)
```

## Feature map

| Area | Features | Where |
|---|---|---|
| **Loading** | files and strings; CR+LF files on every compiler; a custom separator (`:`); the file name remembered | [Loading, saving, printing](./files#load) |
| **Syntax** | sections, the global section (options before any header, or under `[]`), `!` `;` `#` comment lines, `;` inline comments kept apart, values on several lines, dotted section names | [The INI syntax](./syntax) |
| **Reading** | one `get` for integers (`I1P`...`I8P`), reals (`R4P`, `R8P`, `R16P`), complex numbers `(re,im)`, logicals and strings; scalars and arrays, any delimiter; strings of any length (`get_string`); defaults | [Reading values](./reading) |
| **Writing** | one `add` to create sections and to add or update options of any supported type, scalars and arrays; reals written as the shortest text that reads back exactly; `del`, `free_options`, `free` | [Writing values](./writing) |
| **Exploring** | sections by number or by name, their list; options by name; `has_section`, `has_option` (which section holds it); positions (`index`, first or last); item counts; loops over a section or the whole file; every pair at once | [Exploring a file](./exploring) |
| **Saving and printing** | save to a file, print to any unit with a prefix; inline comments on request; `iostat` and `iomsg` report the first failure | [Loading, saving, printing](./files#save) |
| **Errors** | an `error` or `iostat` on every call that can fail; a failed `get` leaves its value unchanged, an array is checked whole | [Errors](./errors) |
| **Every method** | the arguments of each type-bound procedure, in tables | [Methods summary](./api-reference) |

## Design

- **Pure Fortran 2003+** with the `string` type of [StringiFor](https://github.com/szaghi/StringiFor); no C bindings.
- **A small data model**: a `file_ini` holds an array of sections, a section an array of options, an option its name,
  its value (as text) and its inline comment. Values are converted only when you `get` them, and formatted when you
  `add` them.

<!--@include: ../diagrams/model.svg-->

- **Unlimited polymorphic values**: `get` and `add` take `class(*)` values, so one generic call covers every type and
  kind; the type of your variable decides the conversion.
- **Failures are reported, never fatal**: no procedure stops your program.

## Limits

- **Reading and writing, not round-tripping**: `save` writes what FiNeR holds: comment lines, blank lines, indentation and
  the layout of multi-line values are not kept, and options are always written as `name = value`, whatever the
  separator read. Inline comments are kept on request.
- **One level of sections**: `[a.b]` is a flat name, not a subsection of `[a]`.
- **Names are case-sensitive**, and a section written twice is not merged (only the first is reached by name).
- **Lists**: `count_values` counts every delimiter, so write lists with single delimiters
  ([details](./reading#arrays)); `add` writes arrays with blanks only.
- **Comments after values**: only `;` starts an inline comment; `!` and `#` there are part of the value.
- **Sizes**: a file is read whole into memory before it is parsed. That is no burden for configuration files: the test
  file `src/tests/huge.ini` (35 593 lines, 1616 sections) loads in a few hundredths of a second with an optimized build.
