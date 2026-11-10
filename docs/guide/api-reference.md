---
title: Methods summary
---

# Methods summary

Every type-bound procedure of `file_ini`, with its arguments. Optional arguments are in square brackets; pass them by
keyword, as the examples do. The pages of the reference explain each group, and the [API pages](/api/) are generated
from the sources.

## The type

```fortran
type :: file_ini
  character(len=:), allocatable, public :: filename   ! set by load(filename=), save(filename=), initialize(filename=)
  ! every other component is private
contains
  ...
endtype file_ini
```

`b = a` copies a `file_ini` (file name, separator, sections and options). `file_ini_autotest()`, a public subroutine of
`finer`, runs a self test that prints its results.

## Loading, saving, printing

| Method | Arguments | |
|---|---|---|
| `load` | `[filename]`, `[source]`, `[separator]`, `[error]` | read a file or a string; [details](./files#load) |
| `save` | `[filename]`, `[retain_comments]`, `[iostat]`, `[iomsg]` | write the file; [details](./files#save) |
| `print` | `unit`, `[pref]`, `[retain_comments]`, `[iostat]`, `[iomsg]` | write to a unit; [details](./files#print) |
| `initialize` | `[filename]` | empty the `file_ini`, set its file name; [details](./files#initialize) |
| `free` | | delete everything |

## Reading

| Method | Arguments | |
|---|---|---|
| `get` (scalar) | `section_name`, `option_name`, `val`, `[error]`, `[default]` | convert a value into `val`; [details](./reading#get) |
| `get` (array) | `section_name`, `option_name`, `val(:)`, `[delimiter]`, `[error]`, `[default(:)]` | convert a list into `val` |
| `get_string` | `section_name`, `option_name`, `val`, `[error]`, `[default]` | a value into an allocatable string; [details](./reading#get-string) |
| `count_values` | `section_name`, `option_name`, `[delimiter]` | function: the number of items of a value |

## Writing

| Method | Arguments | |
|---|---|---|
| `add` (section) | `section_name`, `[error]` | create a section if missing; [details](./writing#add) |
| `add` (scalar, array) | `section_name`, `option_name`, `val` or `val(:)`, `[error]` | create or update an option |
| `del` | `section_name`, `[option_name]` | delete a section, or one of its options; [details](./writing#del) |
| `free_options` | `[section_name]`, `[option_name]` | delete every option, those of a section, or one |

## Exploring

| Method | Arguments | |
|---|---|---|
| `sections_number` | | function: the number of sections; [details](./exploring#sections) |
| `section` | `section_index` | function: the name of a section |
| `get_sections_list` | `list` | subroutine: every section name |
| `has_section` | `section_name` | function: is the section there |
| `has_option` | `option_name`, `[section_name]` | function: is the option in some section, and in which; [details](./exploring#options) |
| `index` | `section_name`, `[option_name]`, `[back]` | function: the position of a section, or of an option in its section |
| `get_items` | `items` | subroutine: every name and value |
| `loop` | `[section_name]`, `option_pairs` | function: the next name and value, `.false.` at the end; [details](./exploring#loop) |

## Argument types

| Argument | Type and intent |
|---|---|
| `section_name`, `option_name` | `character(*)`, in (`inout` for the `section_name` of `has_option`) |
| `val` | `class(*)`: `inout` for `get`, `in` for `add`; `character(len=:), allocatable, inout` for `get_string` |
| `default` | `class(*)`, in (`character(*)` for `get_string`) |
| `delimiter`, `pref`, `filename`, `source` | `character(*)`, in |
| `separator` | `character(1)`, in |
| `unit`, `section_index` | `integer(I4P)`, in |
| `error`, `iostat` | `integer(I4P)`, out |
| `iomsg` | `character(*)`, out |
| `retain_comments`, `back` | `logical`, in |
| `list`, `option_pairs` | `character(len=:), allocatable`, rank 1, out |
| `items` | `character(len=:), allocatable`, rank 2 (`items(n, 2)`), out |

## Error codes

`ERR_OPTION_NAME` (1), `ERR_OPTION_VALS` (2), `ERR_OPTION` (3), `ERR_SECTION_NAME` (4), `ERR_SECTION_OPTIONS` (5),
`ERR_SECTION` (6), `ERR_SOURCE_MISSING` (7): see [Errors](./errors) for which method returns which.
