---
title: Exploring a file
---

# Exploring a file

## Sections {#sections}

| Call | Returns |
|---|---|
| `fini%sections_number()` | the number of sections, the global one included |
| `fini%section(i)` | the name of the section `i` (from 1, in file order); `i` must be in `1:sections_number()` |
| `call fini%get_sections_list(list)` | `list`, `character(len=:), allocatable :: list(:)`: every name, all of the same length (the longest), blank-padded; not allocated when there is no section |
| `fini%has_section(section_name=)` | `.true.` if the section exists |
| `fini%index(section_name= [, back=])` | the position of the section, `0` if missing; with `back=.true.`, the last of the sections with that name |

The global section, when there is one, is the section `1` and its name is `''`.

## Options {#options}

| Call | Returns |
|---|---|
| `fini%has_option(option_name= [, section_name=])` | `.true.` if some section holds the option; `section_name` (inout, long enough) receives the name of the first one |
| `fini%index(section_name=, option_name= [, back=])` | the position of the option inside its section, `0` if missing; with `back=.true.`, the last option with that name in the last section with that name |
| `fini%count_values(section_name=, option_name= [, delimiter=])` | the number of items of the value (see [Arrays](./reading#arrays)), `0` if missing |
| `call fini%get_items(items)` | `items(1:n, 1:2)`, `character(len=:), allocatable`: the name and the value of every option of every section, in order |

## Loops {#loop}

```fortran
character(len=:), allocatable :: pair(:)

do while (fini%loop(section_name='time', option_pairs=pair))   ! the options of a section
  print '(A)', trim(pair(1))//' = '//trim(pair(2))
enddo
do while (fini%loop(option_pairs=pair))                        ! every option of every section
  ...
enddo
```

- `loop` returns `.true.` with the next pair, `pair(1)` the name and `pair(2)` the value (both padded to the same length),
  then `.false.` when the options are over, ready for a new loop.
- The position of a loop is kept in `fini` (so `fini` must be a variable, `intent(inout)` in a procedure): loops over
  different sections or different files do not interfere, but a loop must run to its end before a new one starts over
  the same section, or over the whole file.
- A section without options ends its loop at once; the loop over the whole file skips it.

## Examples

- Tutorial: [4. Walking the file](/manual/tutorial/04-walking).
- Cookbook: [check that a section or an option exists](/manual/cookbook#check-that-a-section-or-an-option-exists-and-find-its-section),
  [list the sections](/manual/cookbook#list-the-sections), [loops](/manual/cookbook#loop-over-the-options-of-a-section-or-of-the-whole-file),
  [every pair at once](/manual/cookbook#get-every-option-name-and-value-at-once),
  [repeated options](/manual/cookbook#repeated-options-the-first-one-wins).
