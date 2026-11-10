---
title: Cookbook
---

# Cookbook

Short answers to "how do I ...?". Each recipe is a complete program with its real output; the
[reference](/guide/features#feature-map) has the details. The files they read are in
[`docs/examples/files`](https://github.com/szaghi/FiNeR/tree/master/docs/examples/files): `heat.ini` of the
[tutorial](./index), `rod.ini`, `settings.ini`, `colon.cfg`.

[[toc]]

## Reading

### Parse a string

<<< @/examples/snippets/parse_string.f90

<<< @/examples/output/parse_string.ansi{ansi}

Lines are separated by `new_line('a')`. The same `file_ini` can `load` again: the new sections replace the old ones.

### Parse a file, and stop on errors

<<< @/examples/snippets/parse_file.f90

<<< @/examples/output/parse_file.ansi{ansi}

`error` is the I/O status of the read, not zero when the file cannot be read; compare it with `0`.

### Read an integer or a real of any kind

<<< @/examples/snippets/read_number.f90

<<< @/examples/output/read_number.ansi{ansi}

The kind of `val` decides the conversion; an integer that does not fit its kind is an error (`ERR_OPTION_VALS`), never a wrapped value. `1.0e-4` is not exact in `R4P`.

### Read a logical

<<< @/examples/snippets/read_logical.f90

<<< @/examples/output/read_logical.ansi{ansi}

Logicals follow Fortran: a first letter `T` or `F` (any case), optionally after a point. `yes` and `1` are not logicals.

### Read a list of numbers of unknown length

<<< @/examples/snippets/read_list.f90

<<< @/examples/output/read_list.ansi{ansi}

`count_values` first, then an array of that size. The list can span several lines.

### Read a list with another delimiter

<<< @/examples/snippets/read_list_delimiter.f90

<<< @/examples/output/read_list_delimiter.ansi{ansi}

Pass the same `delimiter` to `count_values` and to `get`. Blanks around numbers are ignored. A `character` array receives one item per element.

### Read complex numbers

<<< @/examples/snippets/read_complex.f90

<<< @/examples/output/read_complex.ansi{ansi}

Complex values are written `(re,im)`; in a list, each value in parentheses is one item, whatever separates them.

### Read a string of any length

<<< @/examples/snippets/read_string.f90

<<< @/examples/output/read_string.ansi{ansi}

`get_string` allocates `val` to the length of the value; `get` into a fixed-length string truncates or pads it.

### Give a setting a default

<<< @/examples/snippets/default_value.f90

<<< @/examples/output/default_value.ansi{ansi}

`default` is used only when `get` fails, and `error` still says why. An array `get` takes an array default of the same size.

### Quadruple precision reals

<<< @/examples/snippets/quad_precision.f90

<<< @/examples/output/quad_precision.ansi{ansi}

`R16P` is quadruple precision when PENF is built with `PENF_R16P` (FoBiS always, CMake when the compiler supports it, fpm never).

## Exploring

### Check that a section or an option exists, and find its section

<<< @/examples/snippets/check_exists.f90

<<< @/examples/output/check_exists.ansi{ansi}

`has_option` searches every section; its optional `section_name` receives the first that holds the option. `index` checks one section.

### Repeated options, the first one wins

<<< @/examples/snippets/repeated_options.f90

<<< @/examples/output/repeated_options.ansi{ansi}

An option written twice is kept twice: `get` reads the first, `index(back=.true.)` finds the last, `loop` returns all of them.

### List the sections

<<< @/examples/snippets/list_sections.f90

<<< @/examples/output/list_sections.ansi{ansi}

All the names have the length of the longest; the global section is the empty name.

### Loop over the options of a section, or of the whole file

<<< @/examples/snippets/loop_options.f90

<<< @/examples/output/loop_options.ansi{ansi}

A loop keeps its position in the `file_ini`: let it run to its end before starting another over the same options.

### Get every option name and value at once

<<< @/examples/snippets/get_items.f90

<<< @/examples/output/get_items.ansi{ansi}

`items(i, 1)` is a name, `items(i, 2)` its value, for every option of every section in order.

## Writing

### Build a file in code and save it

<<< @/examples/snippets/build_file.f90

<<< @/examples/output/build_file_out.ansi{ansi}

`add` creates the sections as needed, in the order of the calls.

### Change a value and save the file

<<< @/examples/snippets/update_value.f90

<<< @/examples/output/update_value_out.ansi{ansi}

An `add` of an existing option updates it in place, keeping its position and its inline comment.

### Write arrays and complex numbers

<<< @/examples/snippets/write_arrays.f90

<<< @/examples/output/write_arrays.ansi{ansi}

Arrays are written with one blank between items; strings in an array are trimmed.

### How reals are written

<<< @/examples/snippets/real_format.f90

<<< @/examples/output/real_format.ansi{ansi}

The shortest text that reads back to the same value: plain decimals for exponents from -5 to 15, scientific notation beyond, `huge` included.

### Options outside any section

<<< @/examples/snippets/global_section.f90

<<< @/examples/output/global_section.ansi{ansi}

The global section is the empty name, always the first section, saved without a header.

### Delete options and sections, or empty them

<<< @/examples/snippets/delete_options.f90

<<< @/examples/output/delete_options.ansi{ansi}

`del` removes, `free_options` empties and keeps the sections, `free` resets everything. Deleting what does not exist does nothing.

### Keep the inline comments when saving

<<< @/examples/snippets/keep_comments.f90

<<< @/examples/output/keep_comments_out.ansi{ansi}

Only inline comments (after `;`) are kept, and only with `retain_comments=.true.`; comment lines are not written back.

### Print a file into a log, with a prefix

<<< @/examples/snippets/print_prefix.f90

<<< @/examples/output/print_prefix.ansi{ansi}

`print` takes any open unit; `pref` starts every line.

### Copy a file_ini, then change the copy only

<<< @/examples/snippets/copy_file.f90

<<< @/examples/output/copy_file.ansi{ansi}

Assignment makes an independent copy.

### Remember the file name, save and load without passing it

<<< @/examples/snippets/reload_file.f90

<<< @/examples/output/reload_file.ansi{ansi}

`initialize`, `load` and `save` store the file name in `filename`, a public member; `save()` and `load()` without arguments use it.

## Dialects

### Read a file that uses another separator

<<< @/examples/snippets/custom_separator.f90

<<< @/examples/output/custom_separator.ansi{ansi}

The separator is used to read only: `print` and `save` always write `name = value`.

### Values on several lines

<<< @/examples/snippets/multi_line.f90

<<< @/examples/output/multi_line.ansi{ansi}

A line without the separator continues the option above it, joined with one blank; comment lines in between are skipped.

### Subsections as dotted names

<<< @/examples/snippets/subsections.f90

<<< @/examples/output/subsections.ansi{ansi}

Sections are flat: `[solver.linear]` is one name. A prefix test finds the subsections.

## Errors

### Check every setting and report all the bad ones

<<< @/examples/snippets/report_bad_values.f90

<<< @/examples/output/report_bad_values.ansi{ansi}

`ERR_OPTION_VALS` means the value is there but is not of the type asked; any other non-zero `error` means it is missing.

### Check that a file was saved

<<< @/examples/snippets/save_errors.f90

<<< @/examples/output/save_errors.ansi{ansi}

`iostat` is the status of the first operation that failed; `iomsg` holds the message of the compiler.
