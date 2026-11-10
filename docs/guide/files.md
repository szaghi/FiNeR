---
title: Loading, saving, printing
---

# Loading, saving, printing

## `load` {#load}

```fortran
call fini%load([filename=] [, source=] [, separator=] [, error=])
```

| Argument | Type | |
|---|---|---|
| `filename` | `character(*)`, in | the file to read; it is stored in `fini%filename` |
| `source` | `character(*)`, in | a string holding the whole file, lines separated by `new_line('a')` |
| `separator` | `character(1)`, in | the separator of names and values, `=` by default |
| `error` | `integer(I4P)`, out | `0`, the I/O status of a failed read, or `ERR_SOURCE_MISSING` |

- `filename` takes precedence over `source`. With neither, `load` reads `fini%filename` if it is set (by an earlier
  `load`, `save` or `initialize`, or by hand).
- The new sections replace all the sections of `fini`. If the file cannot be read, `fini` keeps its sections and
  `error` holds the I/O status (not zero).
- `error` is `ERR_SOURCE_MISSING` when there is nothing to read: no source, or a source with no option and no section.
- The separator is remembered by `fini` for the following `load` calls that do not pass one.

<!--@include: ../diagrams/pipeline.svg-->

## `save` {#save}

```fortran
call fini%save([filename=] [, retain_comments=] [, iostat=] [, iomsg=])
```

| Argument | Type | |
|---|---|---|
| `filename` | `character(*)`, in | the file to write (created or replaced); it is stored in `fini%filename` |
| `retain_comments` | `logical`, in | write the inline comments back, `.false.` by default |
| `iostat` | `integer(I4P)`, out | `0`, or the status of the first operation that failed |
| `iomsg` | `character(*)`, out | the message of that failure |

- Without `filename`, `save` writes `fini%filename`. With neither, nothing is written and `iostat` is
  `ERR_SOURCE_MISSING`.
- A `fini` without sections writes nothing: the file is neither created nor truncated, and `iostat` is `0`.
- `iostat` is the status of the first `open`, `write` or `close` that failed: a file that cannot be opened (a missing
  directory, no permission) is reported, and nothing is written after it.
- [What `save` writes](./syntax#what-save-writes): options as `name = value`, comment lines dropped, continuations joined.

## `print` {#print}

```fortran
call fini%print(unit= [, pref=] [, retain_comments=] [, iostat=] [, iomsg=])
```

| Argument | Type | |
|---|---|---|
| `unit` | `integer(I4P)`, in | an open unit, `6` (or `output_unit`) for the standard output |
| `pref` | `character(*)`, in | a prefix written at the start of every line |
| `retain_comments` | `logical`, in | print the inline comments, `.false.` by default |
| `iostat`, `iomsg` | out | as for `save`: `0` when nothing failed (also when there is nothing to print) |

`print` writes what `save` writes, each option indented by two blanks, each line after `pref`.

## The file name, `initialize` and `free` {#initialize}

- `fini%filename` is a public `character(len=:), allocatable` member: `load(filename=)` and `save(filename=)` set it, and
  you can set it by hand.
- `initialize([filename=])` empties `fini` (as `free`) and sets `filename`.
- `free()` deletes every section and option, the file name, and the separator (back to `=`).

## Copying {#assignment}

`b = a` makes `b` an independent copy of `a`: its file name, separator, sections and options. Changing one does not
change the other.

## Examples

- Tutorial: [1. A first file](/manual/tutorial/01-first-file), [5. Editing and saving](/manual/tutorial/05-editing).
- Cookbook: [parse a string](/manual/cookbook#parse-a-string), [parse a file](/manual/cookbook#parse-a-file-and-stop-on-errors),
  [keep the inline comments](/manual/cookbook#keep-the-inline-comments-when-saving),
  [remember the file name](/manual/cookbook#remember-the-file-name-save-and-load-without-passing-it),
  [copy a file_ini](/manual/cookbook#copy-a-file-ini-then-change-the-copy-only),
  [check that a file was saved](/manual/cookbook#check-that-a-file-was-saved).
