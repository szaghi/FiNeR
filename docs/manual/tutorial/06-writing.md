# 6. Writing from scratch

At the end of the run `heat` writes its results: a new file, built option by option.

<<< @/examples/snippets/heat_6.f90

## Building the file

<<< @/examples/snippets/heat_6-build.f90

- `add(section_name=)` alone creates an empty section; `add` with an option creates its section when needed, so the first
  call is optional.
- `section_name=''` is the global section: it is always the first section and is written without a header.
- Integers are written without a sign when positive, logicals as `T` and `F`, reals as the shortest exact text
  (`3.2E-9` for an `R4P`), complex numbers as `(re,im)`.
- An array is written as its values separated by one blank; strings are trimmed (`['p1', 'p2', 'p3']`).

## Printing it

<<< @/examples/snippets/heat_6-print.f90

`print` writes to any unit, each line prefixed by `pref`; it indents the options by two blanks (a saved file does not).

<<< @/examples/output/heat_6.ansi{ansi}

## The file

<<< @/examples/output/heat_6_file.ansi{ansi}

::: tip What you learned
`add` builds a file from nothing, `print(unit=, pref=)` shows it, `save` writes it. Reference:
[Writing values](/guide/writing).
:::

Next: [7. Other INI dialects](./07-dialects).
