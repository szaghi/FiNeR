# 1. A first file

A `file_ini` holds a whole INI file: its sections, and the options of each section. Everything comes from one module,
`finer`; the kinds (`I4P`, `R8P`, ...) come from [PENF](https://github.com/szaghi/PENF), a dependency of FiNeR.

<<< @/examples/snippets/heat_1.f90

- `load(source=)` parses a string: lines are separated by `new_line('a')`. A header `[name]` starts a section and each
  `name = value` line is an option of the section above it.
- `get(section_name=, option_name=, val=)` converts the value into `val`. `val` is unlimited polymorphic: the same call
  reads an integer, a real or a string, and the type of `val` decides the conversion.
- `print(unit=)` writes the file as FiNeR holds it, with the options indented.

## Running it

<<< @/examples/output/heat_1.ansi{ansi}

::: tip What you learned
`load` a source, `get` typed values, `print` the result. Reference: [Loading, saving, printing](/guide/files),
[Reading values](/guide/reading).
:::

Next: [2. Reading the case file](./02-reading).
