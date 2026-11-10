# 8. Robust input

A case file is written by hand, so `heat` must say what is wrong with it instead of running with a bad setting.

<<< @/examples/snippets/heat_8.f90

## A file that cannot be read

<<< @/examples/snippets/heat_8-missing-file.f90

## Values that cannot be read

<<< @/examples/snippets/heat_8-get-errors.f90

A `get` that fails sets `error` and **leaves `val` unchanged** (or sets it to `default=`):

| `error` | Constant | When |
|---|---|---|
| 3 | `ERR_OPTION` | the section or the option is missing |
| 2 | `ERR_OPTION_VALS` | the option has no value; the value is not of the type of `val` (`copper` into an integer, `3.5` into an integer); it is out of the range of the kind (`8960` into an `I1P`); an array `val` is too small for the list; `val` is of a type FiNeR does not support |

An array is checked whole before it is touched: if one value is bad, none is stored.

The constants come from `finer`, in lower case like any Fortran name:

<<< @/examples/snippets/heat_8-named-errors.f90

## Files that cannot be saved

<<< @/examples/snippets/heat_8-save-errors.f90

`save` without a file name (neither passed nor set by `load`) returns `ERR_SOURCE_MISSING` (7) with a message; a file
that cannot be opened or written returns the I/O status of the compiler, which is not zero.

## Running it

<<< @/examples/output/heat_8.ansi{ansi}

::: tip What you learned
Check `error` after `load` and `get`, `iostat` after `save`; a failed `get` never changes `val`. Reference:
[Errors](/guide/errors).
:::

This is the end of the tutorial. The [cookbook](../cookbook) has short recipes for everyday tasks.
