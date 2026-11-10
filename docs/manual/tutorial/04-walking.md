# 4. Walking the file

A generic tool cannot know the names in advance: it walks the sections and options it finds.

<<< @/examples/snippets/heat_4.f90

## The sections

<<< @/examples/snippets/heat_4-sections.f90

Sections are numbered from 1 in the order of the file, the global section first. `section(i)` is the name of the
section `i`; `get_sections_list` gives all the names at once, as an array of strings of the same length.

## Looping over options

<<< @/examples/snippets/heat_4-loop-section.f90

`loop` returns `.true.` and the next pair `[name, value]` until the options are over, then `.false.`. The state of the
loop is kept in the `file_ini` itself, so a loop must run to its end before starting another over the same section.

<<< @/examples/snippets/heat_4-loop-all.f90

Without `section_name`, `loop` goes over every option of every section.

## All pairs at once, and indexes

<<< @/examples/snippets/heat_4-items.f90

<<< @/examples/snippets/heat_4-index.f90

`get_items` returns every pair in an array `items(1:n, 1:2)`. `index` returns the position of a section, or of an option
inside its section, and `0` when it is missing. `count_values` counts the values of an option.

## Running it

<<< @/examples/output/heat_4.ansi{ansi}

::: tip What you learned
`sections_number`, `section(i)`, `get_sections_list`, `loop`, `get_items`, `index`, `count_values`. Reference:
[Exploring a file](/guide/exploring).
:::

Next: [5. Editing and saving](./05-editing).
