---
title: Reading values
---

# Reading values

## `get` {#get}

```fortran
call fini%get(section_name=, option_name=, val= [, error=] [, default=])                  ! a scalar
call fini%get(section_name=, option_name=, val= [, delimiter=] [, error=] [, default=])   ! an array
```

| Argument | Type | |
|---|---|---|
| `section_name` | `character(*)`, in | the section, `''` for the global one; blanks around it are ignored |
| `option_name` | `character(*)`, in | the option; blanks around it are ignored |
| `val` | `class(*)`, inout | a scalar or a rank-1 array of a supported type: the type decides the conversion |
| `delimiter` | `character(*)`, in | arrays only: the separator of the items, a blank by default |
| `error` | `integer(I4P)`, out | `0`, `ERR_OPTION` (missing) or `ERR_OPTION_VALS` (cannot convert) |
| `default` | `class(*)`, in | the value given to `val` when `get` fails; an array of the size of `val` for arrays |

On failure `val` is left unchanged, or set to `default` (see [Errors](./errors)).

### Supported types

| `val` | The text must be | Example |
|---|---|---|
| `integer(I1P)`, `I2P`, `I4P`, `I8P` | an optional sign and decimal digits, in the range of the kind | `64`, `-3`, `+42`, `007` |
| `real(R4P)`, `R8P`, `R16P` | a Fortran real: digits, sign, point, exponent letter `e`, `d` or `q` | `0.5`, `1e-4`, `1.0d0`, `5` |
| `complex(R4P)`, `R8P`, `R16P` | `(re,im)`, each part a real | `(1.5,-0.25)` |
| `logical` | a Fortran logical: first letter `T`/`t` or `F`/`f`, optionally after a point | `.true.`, `T`, `true`, `.F.`, `false` |
| `character(*)` | anything: the value is copied, truncated or padded to `len(val)` | `copper` |

- A scalar `get` of a list reads its first item (`origin = 0.0 1.0 2.0` into a real gives `0.0`) for integers, reals
  and logicals; a `character` scalar receives the whole text; a scalar complex of a list of complex numbers fails.
- `yes`, `on`, `1` are not logicals; `3.5`, `1e3` are not integers; `1m` is not a real.
- `R16P` is a quadruple precision real only when PENF is compiled with `PENF_R16P`: always with FoBiS, with CMake when
  the compiler supports it, never with fpm (there `R16P` is the kind of `R8P`).
- Any other type of `val` (a derived type, for one) gives `ERR_OPTION_VALS`.

### Arrays

- The items are split by `delimiter` (a blank by default). Repeated delimiters count as one: `1  2` and `1,,2` are
  two items.
- `val` must hold all of them: `size(val)` at least the number of items, otherwise `ERR_OPTION_VALS` and `val`
  unchanged. With more room than items, the extra elements are left unchanged.
- Every item is checked before any is stored: one bad item and `val` is untouched.
- For complex arrays the items are the groups in parentheses, whatever lies between them.
- `count_values(section_name=, option_name= [, delimiter=])` returns the number of items (`0` for a missing option), to
  allocate `val` first.

::: warning `count_values` counts every delimiter
`count_values` returns the number of delimiters plus one, so with repeated delimiters it counts more items than `get`
reads: `1  2` counts 3 and reads 2, and the last element of an array allocated to fit is left unchanged. Write lists
with single delimiters (continuation lines are joined with one blank, so they are safe), or initialize the array.
:::

## `get_string` {#get-string}

```fortran
call fini%get_string(section_name=, option_name=, val= [, error=] [, default=])
```

`val` is `character(len=:), allocatable`: it is (re)allocated to the length of the value, so it needs no allocation
before the call. `default` is a `character(*)`. It cannot be part of the `get` generic, whose `class(*)` specific would
make the call ambiguous.

## Defaults {#defaults}

`default=` is used only when the `get` fails, for any reason: missing section or option, empty value, value that cannot
be converted. `error` still reports the failure. The default can be of any type that can be assigned to `val`:

| `val` | accepts a `default` of type |
|---|---|
| complex | complex, real or integer, any kind |
| real | real or integer, any kind |
| integer | integer of any kind, within the range of `val` |
| logical | logical |
| character | character |

Any other combination leaves `val` unchanged.

## Examples

- Tutorial: [2. Reading the case file](/manual/tutorial/02-reading), [3. Optional settings](/manual/tutorial/03-defaults).
- Cookbook: [numbers](/manual/cookbook#read-an-integer-or-a-real-of-any-kind),
  [logicals](/manual/cookbook#read-a-logical), [lists](/manual/cookbook#read-a-list-of-numbers-of-unknown-length),
  [another delimiter](/manual/cookbook#read-a-list-with-another-delimiter),
  [complex numbers](/manual/cookbook#read-complex-numbers), [strings](/manual/cookbook#read-a-string-of-any-length),
  [defaults](/manual/cookbook#give-a-setting-a-default), [quadruple precision](/manual/cookbook#quadruple-precision-reals).
