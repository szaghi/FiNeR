---
title: Writing values
---

# Writing values

## `add` {#add}

```fortran
call fini%add(section_name= [, error=])                              ! a section
call fini%add(section_name=, option_name=, val= [, error=])          ! an option, scalar or array val
```

| Argument | Type | |
|---|---|---|
| `section_name` | `character(*)`, in | the section, `''` for the global one; stored without leading and trailing blanks |
| `option_name` | `character(*)`, in | the option |
| `val` | `class(*)`, in | a scalar or a rank-1 array of a supported type |
| `error` | `integer(I4P)`, out | `0`, or `ERR_OPTION_VALS` for a `val` of a type FiNeR does not support |

- A section that does not exist is created: at the end of the file, or first if it is the global section. An existing
  section is left as it is.
- An option that exists in the section is updated in place (its inline comment is kept); a new one is appended to the
  section. An unsupported `val` changes nothing.
- Arrays are written with a blank between items; `add` has no `delimiter` argument.

## How values are written {#how-values-are-written}

| `val` | Written as | Example |
|---|---|---|
| integer, any kind | decimal digits, `-` for negative numbers only | `4000`, `-3` |
| real, any kind | the shortest text that reads back to the same number: plain decimals for exponents from -5 to 15, scientific notation beyond | `0.1`, `0.00005`, `6.02214076E+23`, `-1.0E-12` |
| complex, any kind | `(re,im)`, each part as a real | `(1.5,-0.25)` |
| logical | `T` or `F` | `T` |
| character | a scalar as it is, trailing blanks included (a later `load` drops them); each item of an array trimmed | `copper` |
| array | the items, one blank between them | `0.1 0.25 0.4` |

Every real written by `add` is read back by `get` to the same value, `huge` included. FiNeR uses the compact strings of
[PENF](https://github.com/szaghi/PENF), without the `+` of positive numbers.

## Deleting {#del}

```fortran
call fini%del(section_name=)                          ! a section and all its options
call fini%del(section_name=, option_name=)            ! one option
call fini%free_options()                              ! every option of every section; the sections stay
call fini%free_options(section_name=)                 ! every option of a section; the section stays
call fini%free_options(section_name=, option_name=)   ! one option (as del)
call fini%free()                                      ! everything, the file name included
```

Deleting what does not exist does nothing.

## Examples

- Tutorial: [5. Editing and saving](/manual/tutorial/05-editing), [6. Writing from scratch](/manual/tutorial/06-writing).
- Cookbook: [build a file](/manual/cookbook#build-a-file-in-code-and-save-it),
  [change a value](/manual/cookbook#change-a-value-and-save-the-file),
  [arrays and complex numbers](/manual/cookbook#write-arrays-and-complex-numbers),
  [how reals are written](/manual/cookbook#how-reals-are-written),
  [options outside any section](/manual/cookbook#options-outside-any-section),
  [delete options and sections](/manual/cookbook#delete-options-and-sections-or-empty-them).
