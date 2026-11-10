# 3. Optional settings

Not every setting is in every case file. `heat` gives the missing ones a default, reads strings of any length, and asks
which sections and options exist.

<<< @/examples/snippets/heat_3.f90

## Defaults

<<< @/examples/snippets/heat_3-default.f90

When `get` cannot read a value (the section or the option is missing, or the value cannot be converted) it sets `error`
and leaves `val` unchanged, or sets it to `default=` if one is passed. `default` can be of any type assignable to `val`:
an integer default for a real `val` is fine. An array `get` takes an array `default` of the same size.

## Strings of any length

<<< @/examples/snippets/heat_3-string.f90

`get` into a `character(len=32)` truncates or pads the value. `get_string` (re)allocates a deferred-length string
instead, to the exact length of the value, so it needs no guess. It takes `default=` too.

## What is there

<<< @/examples/snippets/heat_3-presence.f90

`has_section` and `has_option` answer `.true.` or `.false.`. `has_option` searches every section, and returns in its
optional `section_name` the name of the first section holding the option (the argument must be long enough to receive
it).

## Running it

<<< @/examples/output/heat_3.ansi{ansi}

`error` is `3` (`ERR_OPTION`): the option is missing. [Chapter 8](./08-robust) has all the error codes.

::: tip What you learned
`default=` on `get` and `get_string`, `get_string` for strings of any length, `has_section`, `has_option`. Reference:
[Reading values](/guide/reading), [Exploring a file](/guide/exploring).
:::

Next: [4. Walking the file](./04-walking).
