# 2. Reading the case file

The case lives in a file, [`heat.ini`](https://github.com/szaghi/FiNeR/blob/master/docs/examples/files/heat.ini):

<<< @/examples/files/heat.ini{ini}

`heat` loads it and reads its settings as numbers, logicals and strings.

<<< @/examples/snippets/heat_2.f90

## Loading

<<< @/examples/snippets/heat_2-load.f90

`load(filename=)` reads the whole file. `error` is `0` on success, otherwise the I/O status of the read: a missing file
gives a non-zero `error`, and the `file_ini` keeps the sections it held before (its `filename` becomes the new one).

## Typed values

<<< @/examples/snippets/heat_2-typed.f90

- Integers of any kind (`I1P` to `I8P`), reals (`R4P`, `R8P`, and `R16P` where quadruple precision is enabled),
  complex numbers, logicals and strings: the type of `val` picks the conversion.
- A logical is written as Fortran reads it: `.true.`, `T`, `true`, `.F.`, `false` (see the
  [cookbook](/manual/cookbook#read-a-logical)).
- `title` comes before the first header, so it belongs to the **global section**, whose name is the empty string:
  `section_name=''`.
- Inline comments (`length = 0.5 ; metres`) are not part of the value.

## Lists

<<< @/examples/snippets/heat_2-arrays.f90

An array `val` reads a list of values separated by blanks. `count_values` gives the number of values first, so the array
can be allocated to fit; `probes` is written on three lines, and continuation lines (lines without `=`) are joined to
the option above them.

## Running it

<<< @/examples/output/heat_2.ansi{ansi}

::: tip What you learned
`load(filename=, error=)`, typed `get`, the global section `''`, lists with `count_values`. Reference:
[Reading values](/guide/reading), [The INI syntax](/guide/syntax).
:::

Next: [3. Optional settings](./03-defaults).
