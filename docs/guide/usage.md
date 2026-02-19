---
title: Usage
---

# Usage

All examples use the modern API: `use finer` and `type(file_ini)`.

## INI Parsing

### Load from a file

Pass `filename=` to read an INI file from disk. The file is opened, parsed, and closed automatically. `opt_sep` is set to `=` unless overridden with `separator=`.

```fortran
use finer
use penf, only: I4P
type(file_ini) :: fini
integer(I4P)   :: error
integer(I4P)   :: port

call fini%load(filename='config.ini', error=error)
if (error /= 0) stop 'failed to load config.ini'

call fini%get(section_name='database', option_name='port', val=port, error=error)
print *, 'port =', port
```

### Load from a string

Pass `source=` to parse INI data held in a character variable — no file on disk required. Useful for embedding default configuration in source code or for testing.

```fortran
use finer
use penf, only: I4P
type(file_ini)                :: fini
character(len=:), allocatable :: source
integer(I4P)                  :: error
character(64)                 :: host

source = '[database]'//new_line('A')// &
         'host = localhost'//new_line('A')// &
         'port = 5432'//new_line('A')// &
         '[logging]'//new_line('A')// &
         'level = info'

call fini%load(source=source, error=error)
call fini%get(section_name='database', option_name='host', val=host, error=error)
print *, trim(host)   ! localhost
```

::: info
If both `filename=` and `source=` are supplied, `filename` takes priority.
:::

### Automatic section and option detection

FiNeR scans the input and discovers all sections and options without any schema or pre-declaration. The section count is available in `fini%Ns`; use it to iterate programmatically.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: sec_name
integer                       :: s

call fini%load(filename='config.ini')
print *, fini%Ns, 'section(s) found'

do s = 1, fini%Ns
  sec_name = fini%section(s)
  print *, '  [', sec_name, ']'
end do
```

::: tip
`fini%Ns` is a public read-only member. Do not assign to it directly — it is managed internally by `add` and `del`.
:::

### Multi-line option values

A line that contains no `=` (and is not a section header or comment) is treated as a continuation of the previous option. FiNeR concatenates it — separated by a space — to the preceding value.

```ini
[section-1]
option-2 = 2.
           3.
           4.
```

```fortran
use finer
use penf, only: R4P, I4P
type(file_ini)         :: fini
real(R4P), allocatable :: values(:)
integer(I4P)           :: Nv, error

call fini%load(filename='config.ini')

Nv = fini%count_values(section_name='section-1', option_name='option-2')
allocate(values(1:Nv))
call fini%get(section_name='section-1', option_name='option-2', val=values, error=error)
print *, values   ! 2.0  3.0  4.0
```

### Inline comments

Any text following `;` on an option line is treated as an inline comment and stripped before the value is stored.

```ini
[database]
port = 5432 ; default PostgreSQL port
host = localhost ; change for remote server
```

```fortran
use finer
use penf, only: I4P
type(file_ini) :: fini
integer(I4P)   :: port

call fini%load(filename='config.ini')
call fini%get(section_name='database', option_name='port', val=port)
print *, port   ! 5432  (comment stripped)
```

::: tip
Full-line comments use `!`, `;`, or `#` as the first non-blank character. Inline comments use `;` after the value.
:::

### Configurable option separator

The default separator between option name and value is `=`. Use `separator=` to override it for files that use `:` or another character.

```ini
[server]
host: 192.168.1.1
port: 8080
```

```fortran
use finer
type(file_ini) :: fini
character(64)  :: host

call fini%load(filename='config.ini', separator=':')
call fini%get(section_name='server', option_name='host', val=host)
print *, trim(host)   ! 192.168.1.1
```

The separator can also be set directly: `fini%opt_sep = ':'`.

---

## INI Generation

### Add sections and options programmatically

Use `add` to build an INI structure in memory. Adding a section that already exists is a no-op. Adding an option to a non-existent section creates the section automatically.

```fortran
use finer
type(file_ini) :: fini

call fini%add(section='network')
call fini%add(section='network', option='host', val='localhost')
call fini%add(section='network', option='port', val=8080)
call fini%add(section='logging')   ! section only, no options yet
```

### Set option values of any intrinsic type

`val=` is unlimited polymorphic. Pass integer, real, logical, or character scalars of any kind, including all [PENF](https://github.com/szaghi/PENF) kind parameters.

```fortran
use finer
use penf, only: I4P, I8P, R4P, R8P
type(file_ini) :: fini

call fini%add(section='types')
call fini%add(section='types', option='int32',  val=42_I4P)
call fini%add(section='types', option='int64',  val=123456789_I8P)
call fini%add(section='types', option='real32', val=3.14_R4P)
call fini%add(section='types', option='real64', val=2.71828_R8P)
call fini%add(section='types', option='flag',   val=.true.)
call fini%add(section='types', option='label',  val='production')
```

### Array-valued options

Pass a 1-D array as `val=`. Values are written space-separated. Use the optional `delimiter=` argument to choose a different separator.

```fortran
use finer
use penf, only: R8P
type(file_ini) :: fini

call fini%add(section='sec-foo')
call fini%add(section='sec-foo', option='array',  val=[1, 2, 3, 4])
call fini%add(section='sec-foo', option='floats', val=[1.0_R8P, 2.5_R8P, 3.0_R8P])
call fini%add(section='sec-foo', option='bools',  val=[.true., .false., .true.])
```

The resulting INI content:

```ini
[sec-foo]
array = +1 +2 +3 +4
floats = +1.00000000000000E+000 +2.50000000000000E+000 +3.00000000000000E+000
bools = T F T
```

::: tip
`count_values` counts space-separated tokens, so it works correctly with array-valued options written by `add`.
:::

### Remove sections and options

`del` removes an option from a section, or an entire section together with all its options.

```fortran
use finer
type(file_ini) :: fini

call fini%load(filename='config.ini')

call fini%del(section_name='sec-foo', option_name='bar')   ! remove one option
call fini%del(section_name='sec-bar')                      ! remove whole section
```

::: warning
Deleting a section removes all of its options. `fini%Ns` is decremented automatically.
:::

### Save to a named file

`save` writes the in-memory structure to a file. If `filename=` is omitted, the value of `fini%filename` is used (set automatically by a previous `load` call, or assignable directly).

```fortran
use finer
use penf, only: R8P
type(file_ini) :: fini
integer        :: iostat

call fini%add(section='sec-foo')
call fini%add(section='sec-foo', option='bar', val=-32.1_R8P)
call fini%add(section='sec-foo', option='baz', val=' hello FiNeR! ')
call fini%add(section='sec-bar')
call fini%add(section='sec-bar', option='bools', val=[.true., .false., .false.])

call fini%save(filename='foo.ini', iostat=iostat)
if (iostat /= 0) stop 'save failed'
```

The generated `foo.ini`:

```ini
[sec-foo]
bar = -0.321000000000000E+002
baz =  hello FiNeR!
[sec-bar]
bools = T F F
```

---

## Introspection

### Check whether a section exists

`has_section` scans the section list and returns `.true.` on the first match.

```fortran
use finer
type(file_ini) :: fini

call fini%load(filename='config.ini')

if (fini%has_section(section_name='database')) then
  print *, 'database section found'
end if
```

### Check whether an option exists

`has_option` searches all sections (or a specific one) for the named option. The optional `section_name=` argument restricts the search; without it, all sections are scanned.

```fortran
use finer
type(file_ini) :: fini
character(64)  :: container
logical        :: found

call fini%load(filename='config.ini')

! Search everywhere
found = fini%has_option(option_name='host')

! Search in a specific section
found = fini%has_option(option_name='host', section_name='database')

! Find which section contains the first occurrence
found = fini%has_option(option_name='host', section_name=container)
if (found) print *, 'host found in: ', trim(container)
```

::: info
When `section_name` is used as an output buffer the returned value is truncated to the declared length if the actual section name is longer.
:::

### Get a section name by index

`section(i)` returns the name of the i-th section. Combine with `fini%Ns` to iterate over all sections without hard-coding their names.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: sec_name
integer                       :: s

call fini%load(filename='config.ini')

do s = 1, fini%Ns
  sec_name = fini%section(s)
  print *, 'section ', s, ': ', sec_name
end do
```

### Get the index of a section or option

`index` is the inverse of `section`: given a name, it returns the position in the list, or `0` if not found. It can also locate an option within a section.

```fortran
use finer
type(file_ini) :: fini
integer        :: s, o

call fini%load(filename='config.ini')

s = fini%index(section='database')
if (s > 0) print *, 'database is section #', s

o = fini%index(section='database', option='host')
if (o > 0) print *, 'host is option #', o, ' in database'
```

::: tip
`index` can be used as an integer-valued alternative to `has_section`/`has_option` when you need the position and not just the presence.
:::

### Count values in an option

`count_values` returns the number of space-separated tokens in an option's value. Use it to allocate a receiving array before calling `get`.

```fortran
use finer
use penf, only: I4P
type(file_ini)            :: fini
integer(I4P), allocatable :: arr(:)
integer                   :: Nv

call fini%load(source='[mesh]'//new_line('A')//'dims = 100 200 50')

Nv = fini%count_values(section_name='mesh', option_name='dims')
allocate(arr(1:Nv))
call fini%get(section_name='mesh', option_name='dims', val=arr)
print *, arr   ! 100 200 50
```

### Retrieve all option pairs as an array

`items` returns a `(N, 2)` allocatable character array. Each row contains `[option_name, option_value]` as strings. Useful for generic processing or serialisation.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: it(:,:)
integer                       :: i

call fini%load(filename='config.ini')

it = fini%items(section_name='database')
do i = 1, size(it, dim=1)
  print *, trim(it(i,1)), ' = ', trim(it(i,2))
end do
```

### Iterate over options with a loop

`loop` provides a stateful `do while` iterator. It returns `.true.` and fills `option(:)` with `[name, value]` on each call, then returns `.false.` and resets when the list is exhausted.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: opt(:)
integer                       :: s

call fini%load(filename='config.ini')

! All options in one section
do while (fini%loop(section_name='database', option=opt))
  print *, trim(opt(1)), ' = ', trim(opt(2))
end do

! All options in the entire file
do while (fini%loop(option=opt))
  print *, trim(opt(1)), ' = ', trim(opt(2))
end do

! Loop with section filtering
do s = 1, fini%Ns
  if (fini%section(s) /= 'skip-me') then
    do while (fini%loop(section_name=fini%section(s), option=opt))
      print *, trim(opt(1)), ' = ', trim(opt(2))
    end do
  end if
end do
```

### Pretty-print to a Fortran unit

`print` writes formatted INI output to any Fortran I/O unit. The optional `pref=` adds a fixed string at the start of each line, and `retain_comments=.true.` preserves inline comments.

```fortran
use finer
type(file_ini) :: fini
integer        :: iostat
character(200) :: iomsg

call fini%load(filename='config.ini')

! Print to stdout (unit 6)
call fini%print(unit=6)

! Print with prefix and inline comments
call fini%print(unit=6, pref='|-->', iostat=iostat, iomsg=iomsg, retain_comments=.true.)
if (iostat /= 0) print *, trim(iomsg)
```
