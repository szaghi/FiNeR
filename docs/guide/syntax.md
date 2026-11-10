---
title: The INI syntax
---

# The INI syntax

INI files have no single standard. This page says exactly what FiNeR reads, and what it writes back.

<!--@include: ../diagrams/anatomy.svg-->

## Lines

`load` drops every carriage return, so a file with Windows (CR+LF) line endings reads as one with LF endings, then
reads the source line by line:

| A line that | is |
|---|---|
| starts (after blanks) with `!`, `;` or `#` | a comment, skipped |
| starts (after blanks) with `[` | a section header |
| holds the separator (`=` by default) | an option, `name = value` |
| holds no separator | the continuation of the option above it |
| is blank | ignored |

The separator is set by `load(separator=)`: one character, for example `:`. It is used only to read: `print` and
`save` always write `name = value`.

## Sections

- `[name]` starts the section `name`: the name is what lies between the first `[` and the first `]`, without leading and
  trailing blanks. It can hold any other character, dots and blanks included.
- Names are case-sensitive: `[Grid]` and `[grid]` are two sections.
- The options before the first header belong to the **global section**, whose name is the empty string
  (`section_name=''`). It is always the first section, and is written without a header. An explicit header `[]` (or
  `[ ]`) does not start a section: its options join the global section.
- There is no nesting: `[solver.linear]` is a section named `solver.linear`. Dotted names are the usual way to write
  subsections.
- A header that appears twice starts two sections with the same name: they are not merged, and only the first is reached
  by name. Write each section once.

## Options

- `name = value`: the name is the text before the first separator, the value the text after it, both without leading and
  trailing blanks. The value can hold the separator: `eq = a=b` has the value `a=b`.
- Names are case-sensitive; the blanks around a name are ignored, inside it they count (`my key` is a valid name).
- An option written twice in a section is kept twice: `get` reads the first, `index(back=.true.)` finds the last.
- `name =` with nothing after the separator is an option without a value: `get` returns `ERR_OPTION_VALS` for it.

## Values

- A value is text. `get` converts it into the type of `val` (see [Reading values](./reading)); `add` writes a number,
  a logical or a string as text (see [Writing values](./writing)).
- A list is a value with several items separated by blanks, or by another delimiter given to `get` and
  `count_values` (`delimiter=','`). Complex numbers are written `(re,im)`, and a list of them `(1,2) (3,4)`.
- A value continues on the following lines that hold no separator: they are joined to it with one blank. Comment lines
  in between are skipped.

  ```ini
  probes = 0.1
           0.25      ; probes = 0.1 0.25 0.4
           0.4
  ```

## Comments

- Comment lines (`!`, `;` or `#` first) are skipped, and are not written back by `save`.
- After a value, `;` starts an **inline comment**: it is kept apart from the value, and written back by `print` and
  `save` with `retain_comments=.true.`.
- Inside a value, `!` and `#` are ordinary characters: `url = http://host/page#top` has the value
  `http://host/page#top`.

::: warning Lines without a separator are continuations
A line that holds no separator is always joined to the option above it, whatever it holds: `key` alone on a line
becomes part of the previous value. A `#` or `!` comment written after a continuation value is part of the value too
(`0.4 # last probe` makes the list `... 0.4 # last probe`). Use `;` for comments after values.
:::

## What `save` writes

- Every section in order, the global one first and without header, then `[name]` and its options.
- Each option as `name = value`, not indented, with ` ; comment` when `retain_comments=.true.`.
- Values on one line, continuations joined.
- Comment lines and blank lines are not kept.

See [Loading, saving, printing](./files) for the arguments.
