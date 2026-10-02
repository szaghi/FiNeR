---
title: API Reference
---

# API Reference

FiNeR exposes a single module:

```fortran
use finer
```

The module re-exports everything from the internal modules. The primary user-facing type is `file_ini`.

## `file_ini` type

### Public members

| Member | Type | Description |
|--------|------|-------------|
| `filename` | `character(len=:), allocatable` | Path of the INI file (set automatically by `load`/`save`, or set directly) |

### Public methods

| Method | Description |
|--------|-------------|
| [`free`](#free) | Free all dynamic memory, resetting the object |
| [`load`](#load) | Load INI data from a file or string |
| [`has_option`](#has_option) | Inquire whether an option exists |
| [`has_section`](#has_section) | Inquire whether a section exists |
| [`section`](#section) | Get a section name by index |
| `sections_number` | Return the number of sections |
| [`index`](#index) | Get the index of a named section or option |
| [`count_values`](#count_values) | Count the number of space-separated values in an option |
| [`add`](#add) | Add a section or option (updates value if option already exists) |
| [`get`](#get) | Get an option value, with an optional default |
| [`get_string`](#get_string) | Get an option value as an allocatable string |
| [`del`](#del) | Delete a section or option |
| [`items`](#items) | Return all option name/value pairs as a 2-D array |
| [`loop`](#loop) | Iterate over options with a `do while` loop |
| [`print`](#print) | Pretty-print file data to a Fortran unit |
| [`save`](#save) | Save file data to a physical file |

---

## `free` {#free}

Safely resets a `file_ini` variable, deallocating all sections and options. Use this before re-loading new data into an existing variable.

```fortran
use finer
type(file_ini) :: fini

call fini%load(filename='old.ini')
! ... work with old data ...
call fini%free
call fini%load(filename='new.ini')
```

---

## `load` {#load}

Loads INI data. Either `filename` or `source` must be provided; if both are given, `filename` takes priority.

**Signature:**
```fortran
call fini%load(filename=, source=, separator=, error=)
```

| Argument | Intent | Type | Description |
|----------|--------|------|-------------|
| `filename` | `in`, optional | `character(*)` | Path to the INI file |
| `source` | `in`, optional | `character(*)` | INI content as a string |
| `separator` | `in`, optional | `character(1)` | Option separator (default `=`) |
| `error` | `out`, optional | `integer` | Error code (0 = success) |

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: source

! Load from file with custom separator
call fini%load(filename='config.ini', separator=':')

! Load from an in-memory string
call fini%free
source = '[section-1]'//new_line('A')// &
         'option-1 = one'//new_line('A')// &
         'option-2 = 2.'//new_line('A')// &
         '           3. ; inline comment'//new_line('A')// &
         '[section-2]'//new_line('A')// &
         'option-1 = foo'
call fini%load(source=source)
```

---

## `has_option` {#has_option}

Returns `.true.` if the named option exists anywhere in the file (or within a specific section). Optionally returns the name of the section containing the first match.

```fortran
use finer
type(file_ini)   :: fini
character(100)   :: sec_name
logical          :: found

call fini%load(filename='config.ini')

found = fini%has_option(option_name='host')

! Also get the containing section name
found = fini%has_option(option_name='host', section_name=sec_name)
if (found) print *, 'host is in section: ', trim(sec_name)
```

::: info
`section_name` is a fixed-length buffer. If the actual section name is longer than the buffer, the returned name is truncated.
:::

---

## `has_section` {#has_section}

Returns `.true.` if the named section exists.

```fortran
use finer
type(file_ini) :: fini

call fini%load(filename='config.ini')

if (fini%has_section(section_name='database')) then
  print *, 'database section found'
end if
```

---

## `section` {#section}

Returns the name of the section at position `i`. Use with `sections_number()` to loop over all sections.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: sec_name
integer                       :: s

call fini%load(filename='config.ini')

do s = 1, fini%sections_number()
  sec_name = fini%section(s)
  print *, 'Section: ', sec_name
end do
```

---

## `index` {#index}

Returns the index of a section, or of an option within a section. Returns `0` if not found. The optional `back=.true.` argument returns the last matching occurrence instead of the first.

**Signatures:**
```fortran
i = fini%index(section=, back=)          ! index of section
i = fini%index(section=, option=, back=) ! index of option in section
```

```fortran
use finer
type(file_ini) :: fini
integer        :: s, o

call fini%load(filename='config.ini')

s = fini%index(section_name='database')
if (s > 0) print *, 'database is section #', s

o = fini%index(section_name='database', option_name='host')
if (o > 0) print *, 'host is option #', o, ' in database'
```

---

## `count_values` {#count_values}

Counts the number of space-separated tokens in an option's value. Useful for allocating arrays before calling `get`.

```fortran
use finer
type(file_ini)      :: fini
integer, allocatable :: array(:)
integer              :: Nv

call fini%load(source='[foo]'//new_line('A')//'array = 1 2 3 4 5')

Nv = fini%count_values(section_name='foo', option_name='array')
allocate(array(1:Nv))
call fini%get(section_name='foo', option_name='array', val=array)
print *, array   ! 1 2 3 4 5
```

---

## `add` {#add}

Adds a section, or adds/updates an option within a section. If the section does not exist, it is created automatically. If the option already exists, its value is updated.

**Signatures:**
```fortran
call fini%add(section=)                      ! add section only
call fini%add(section=, option=, val=)       ! add/update option
call fini%add(section=, option=, val=, delimiter=)  ! array with delimiter
```

`val` is unlimited polymorphic — pass any intrinsic scalar or array.

```fortran
use finer
use penf, only: R8P
type(file_ini) :: fini

call fini%add(section_name='sec-foo')
call fini%add(section_name='sec-foo', option_name='bar',   val=-32.1_R8P)
call fini%add(section_name='sec-foo', option_name='baz',   val=' hello FiNeR! ')
call fini%add(section_name='sec-foo', option_name='array', val=[1, 2, 3, 4])
call fini%add(section_name='sec-bar')
call fini%add(section_name='sec-bar', option_name='bools', val=[.true., .false., .false.])
```

---

## `get` {#get}

Retrieves an option value. The receiving variable (`val`) can be a scalar or an array of integer, real, complex, logical or character type. The optional `delimiter` argument specifies the separator between array values (default: space).

```fortran
use finer
use penf, only: I4P
type(file_ini)       :: fini
integer(I4P)         :: error
integer, allocatable :: arr(:)
character(64)        :: host

call fini%load(filename='config.ini')

call fini%get(section_name='database', option_name='host',  val=host,  error=error)
allocate(arr(1:fini%count_values(section_name='foo', option_name='array')))
call fini%get(section_name='foo',      option_name='array', val=arr,   error=error)
if (error == 0) print *, arr
```

::: tip
Always allocate the receiving array to the correct size with `count_values` before calling `get` with an array `val`. If `val` is too small to hold all the values, `get` returns an error and leaves `val` unchanged.
:::

### Errors

`error` is `0` on success. Otherwise `val` is left unchanged and `error` tells why:

| Error | Meaning |
|-------|---------|
| `ERR_OPTION` | the section or the option does not exist |
| `ERR_OPTION_VALS` | the option has no value, the value cannot be converted to the type of `val` (e.g. `abc` or `1.5` read into an integer, an integer too big for the kind of `val`), the type of `val` is not supported, or an array `val` is too small |

### Default values

Pass `default=` to give `val` a fallback value whenever the option cannot be got. `error` still reports what happened, so a default can be told from a value read from the file.

```fortran
use finer
use penf, only: I4P, R8P
type(file_ini) :: fini
integer(I4P)   :: error
real(R8P)      :: radius
integer        :: steps
real(R8P)      :: origin(3)

call fini%load(filename='config.ini')

call fini%get(section_name='cylinder', option_name='radius', val=radius, default=-1._R8P)
call fini%get(section_name='cylinder', option_name='steps',  val=steps,  default=0, error=error)
if (error /= 0) print *, 'steps not found or not valid, using ', steps
call fini%get(section_name='cylinder', option_name='origin', val=origin, default=[0._R8P, 0._R8P, 0._R8P])
```

A complex `val` accepts a complex, real or integer default of any kind. A real `val` accepts a real or integer default of any kind, an integer `val` accepts an integer default of any kind that it can represent, a logical or character `val` accepts a default of the same type. An array `default` must have the same size as `val`. A default that does not fit these rules is ignored: `val` is left unchanged.

### Complex values

Complex values use the Fortran notation, `(real,imaginary)`, with or without blanks inside the parentheses.

```ini
[cylinder]
epsilon = (80., 1.0d-4) ; water permittivity
modes   = (1.0, 2.0) (3.0, 4.0)
```

```fortran
use finer
use penf, only: R8P
type(file_ini)            :: fini
complex(R8P)              :: epsilon
complex(R8P), allocatable :: modes(:)

call fini%load(filename='config.ini')

call fini%get(section_name='cylinder', option_name='epsilon', val=epsilon)
allocate(modes(1:fini%count_values(section_name='cylinder', option_name='modes')))
call fini%get(section_name='cylinder', option_name='modes', val=modes)

call fini%add(section_name='cylinder', option_name='mu', val=(1._R8P, 0._R8P))   ! written as (1.0,0.0)
```

In a list of complex values each value is delimited by its parentheses, so the delimiter (blank by default) can also appear inside a value. `count_values` counts the parenthesised groups when the whole option value starts with `(` and ends with `)`.

A value without parentheses (`80. 1.0d-4`) is not a complex value: reading it into a complex variable returns `ERR_OPTION_VALS`.

---

## `get_string` {#get_string}

Retrieves an option value into a deferred-length allocatable string, which is (re)allocated to the exact length of the value. Unlike `get`, the receiving variable does not need to be allocated, or long enough, before the call.

```fortran
use finer
use penf, only: I4P
type(file_ini)                :: fini
integer(I4P)                  :: error
character(len=:), allocatable :: host

call fini%load(filename='config.ini')

call fini%get_string(section_name='database', option_name='host', val=host, error=error)
call fini%get_string(section_name='database', option_name='user', val=host, default='nobody')
```

If the section or the option does not exist, or the option has no value, an error is returned and `val` is set to `default`, if passed, otherwise it is left unchanged.

::: warning
`get` with a character `val` needs an already allocated variable and silently truncates a value longer than `val`. Prefer `get_string` for strings of unknown length.
:::

---

## `del` {#del}

Deletes a section (and all its options) or a single option within a section.

```fortran
use finer
type(file_ini) :: fini

call fini%load(filename='config.ini')

call fini%del(section_name='sec-foo', option_name='bar')  ! delete one option
call fini%del(section_name='sec-bar')                     ! delete whole section
```

::: warning
Deleting a section removes all of its options.
:::

---

## `items` {#items}

Returns a `(N, 2)` allocatable character array where each row holds `[option_name, option_value]` as strings.

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

---

## `loop` {#loop}

Provides a `do while` iteration over options. Returns `.true.` and fills `option(:)` with `[name, value]` on each call; returns `.false.` when exhausted and resets the internal counter.

The state of a loop is stored in the `file_ini` object, so the object must be a variable (not an `intent(in)` dummy argument), and loops over different sections or different files do not interfere. Run a loop to completion before starting a new one over the same section.

**Signatures:**
```fortran
do while (fini%loop(option_pairs=opt))             ! all options in file
do while (fini%loop(section_name=, option_pairs=)) ! options in one section
```

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: opt(:)

call fini%load(filename='config.ini')

! Iterate over all options in 'database' section
do while (fini%loop(section_name='database', option_pairs=opt))
  print *, trim(opt(1)), ' = ', trim(opt(2))
end do

! Iterate over every option in the entire file
do while (fini%loop(option_pairs=opt))
  print *, trim(opt(1)), ' = ', trim(opt(2))
end do
```

---

## `print` {#print}

Pretty-prints all file data to a Fortran I/O unit.

| Argument | Intent | Type | Description |
|----------|--------|------|-------------|
| `unit` | `in` | `integer` | Fortran unit number (6 = stdout) |
| `pref` | `in`, optional | `character(*)` | Line prefix string |
| `iostat` | `out`, optional | `integer` | I/O status |
| `iomsg` | `out`, optional | `character(*)` | I/O error message |
| `retain_comments` | `in`, optional | `logical` | Print inline comments (default `.false.`) |

```fortran
use finer
type(file_ini) :: fini
integer        :: iostat
character(200) :: iomsg

call fini%load(filename='config.ini')
call fini%print(unit=6, pref='|-->', iostat=iostat, iomsg=iomsg)
call fini%print(unit=6, retain_comments=.true.)
```

---

## `save` {#save}

Saves file data to a physical file. If `filename` is not passed, the value of the `filename` public member is used (which may have been set by a previous `load` call).

| Argument | Intent | Type | Description |
|----------|--------|------|-------------|
| `filename` | `in`, optional | `character(*)` | Output file path |
| `iostat` | `out`, optional | `integer` | I/O status |
| `iomsg` | `out`, optional | `character(*)` | I/O error message |
| `retain_comments` | `in`, optional | `logical` | Write inline comments (default `.false.`) |

```fortran
use finer
use penf, only: R8P
type(file_ini) :: fini
integer        :: iostat
character(200) :: iomsg

call fini%add(section_name='sec-foo', option_name='bar', val=-32.1_R8P)
call fini%save(filename='foo.ini', iostat=iostat, iomsg=iomsg)
call fini%save(filename='foo-with-comments.ini', retain_comments=.true.)
```

---

## Error codes

Defined in module `finer_backend` and re-exported by `finer`:

| Constant | Value | Meaning |
|----------|-------|---------|
| `ERR_OPTION_NAME` | 1 | Bad option name |
| `ERR_OPTION_VALS` | 2 | Bad option values |
| `ERR_OPTION` | 3 | Generic option error |
| `ERR_SECTION_NAME` | 4 | Bad section name |
| `ERR_SECTION_OPTIONS` | 5 | Bad section options |
| `ERR_SECTION` | 6 | Generic section error |
| `ERR_SOURCE_MISSING` | 7 | No source provided to `load` |
