# 7. Other INI dialects

INI has no single standard. `heat` must also read `legacy.cfg`, written by an older tool with `:` as separator, other
comment styles, dotted section names and an explicit empty header.

<<< @/examples/files/legacy.cfg{ini}

<<< @/examples/snippets/heat_7.f90

## Another separator

<<< @/examples/snippets/heat_7-separator.f90

`load(separator=':')` splits names and values at `:`. The separator is one character, and only `load` uses it: `print`
and `save` always write `name = value`.

## Sections

<<< @/examples/snippets/heat_7-sections.f90

- A header `[]` with nothing inside does not start a section: its options join the global section, after the options
  that come before the first header.
- A section name can hold any character but `]`: `[mesh.grid]` is a section named `mesh.grid`. FiNeR has no nesting, so
  dotted names are the usual way to write subsections (see the [cookbook](/manual/cookbook#subsections-as-dotted-names)).

## Comments

<<< @/examples/snippets/heat_7-comments.f90

A line whose first non-blank character is `!`, `;` or `#` is a comment and is skipped. After a value, `;` starts an
inline comment, which FiNeR keeps apart from the value: `print` and `save` write it back with `retain_comments=.true.`.

## Windows line endings

<<< @/examples/snippets/heat_7-crlf.f90

A file with CR+LF line endings reads as with LF endings, on every compiler: `load` drops every carriage return.

## Running it

<<< @/examples/output/heat_7.ansi{ansi}

::: tip What you learned
`separator=`, the three comment characters, inline comments, `[]`, dotted names, CRLF files. Reference:
[The INI syntax](/guide/syntax).
:::

Next: [8. Robust input](./08-robust).
