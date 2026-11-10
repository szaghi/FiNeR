# 5. Editing and saving

After 2000 steps `heat` writes a restart file: the same case, with a smaller time step, more steps and a new section
that says where to restart from.

<<< @/examples/snippets/heat_5.f90

## Updating and appending

<<< @/examples/snippets/heat_5-update.f90

`add(section_name=, option_name=, val=)` sets an option: an option that exists is updated in place, a new one is
appended to its section, and a missing section is created first (at the end of the file). `val` is any supported type,
scalar or array, and FiNeR writes it as text (see [Writing values](/guide/writing#how-values-are-written)).

## Adding and deleting sections

<<< @/examples/snippets/heat_5-sections.f90

`del(section_name=, option_name=)` deletes one option, `del(section_name=)` a whole section with its options.

## Saving

<<< @/examples/snippets/heat_5-save.f90

`save(filename=)` writes the file; without `filename` it uses `filename`, the public member set by `load` or by an earlier
`save`. `iostat` is `0` when everything was written, otherwise the status of the first operation that failed.
`retain_comments=.true.` writes the inline comments back.

<<< @/examples/output/heat_5_file.ansi{ansi}

What a save writes, as you can see above:

- options as `name = value`, without indentation, whatever their layout in the source;
- the inline comments only with `retain_comments=.true.`; comment lines are not kept;
- a value written on several lines as one line;
- reals as the shortest text that reads back to the same number: `5.0e-5_R8P` becomes `0.00005`.

## Reading it back

<<< @/examples/snippets/heat_5-check.f90

<<< @/examples/output/heat_5.ansi{ansi}

::: tip What you learned
`add` updates or appends, `del` deletes, `save(filename=, retain_comments=, iostat=)` writes; reals round-trip
exactly. Reference: [Writing values](/guide/writing), [Loading, saving, printing](/guide/files).
:::

Next: [6. Writing from scratch](./06-writing).
