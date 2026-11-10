---
title: Errors
---

# Errors

FiNeR never stops your program: each procedure that can fail has an optional `error` (or `iostat`) argument, and
reports there. Without it, a failure is silent, so pass it wherever a failure matters.

## The error codes

Defined in `finer_backend` and public from `finer`, as `integer(I4P), parameter`:

| Constant | Value | Returned by |
|---|---|---|
| `ERR_OPTION_VALS` | 2 | `get`: the option has no value, the value cannot be converted to the type of `val` (or is out of its range), an array `val` is too small, or `val` is of an unsupported type. `get_string`: the option has no value. `add`: `val` is of an unsupported type |
| `ERR_OPTION` | 3 | `get`, `get_string`: the section or the option is missing |
| `ERR_SECTION` | 6 | `add`: the section could not be found after being added (not expected) |
| `ERR_SOURCE_MISSING` | 7 | `load`: no source (no `filename`, no `source`, no `fini%filename`), or a source without options and sections. `save`: no file name |
| `ERR_OPTION_NAME` | 1 | not returned by the methods of `file_ini` |
| `ERR_SECTION_NAME` | 4 | not returned by the methods of `file_ini` |
| `ERR_SECTION_OPTIONS` | 5 | not returned by the methods of `file_ini` |

## I/O status

- `load(filename=, error=)`: a file that cannot be read gives its I/O status, a non-zero value that depends on the
  compiler; compare it with `0`, not with a constant.
- `save` and `print`: `iostat` is `0`, `ERR_SOURCE_MISSING` (`save` without a file name), or the I/O status of the first
  `open`, `write` or `close` that failed, with its message in `iomsg`.

## What is left unchanged

- A failed `get` leaves `val` unchanged, or sets it to `default`. For an array, every item is checked before any is
  stored: one bad item and the whole array is untouched.
- A failed `load` (a file that cannot be read) leaves the sections of the `file_ini` as they were.
- A failed `add` (an unsupported type) changes nothing.

## Examples

- Tutorial: [8. Robust input](/manual/tutorial/08-robust).
- Cookbook: [report every bad setting](/manual/cookbook#check-every-setting-and-report-all-the-bad-ones),
  [check that a file was saved](/manual/cookbook#check-that-a-file-was-saved).
