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
| `Ns` | `integer` | Number of sections (managed internally; read to loop over sections) |
| `opt_sep` | `character(1)` | Option name/value separator (default `=`) |

### Public methods

| Method | Description |
|--------|-------------|
| [`free`](#free) | Free all dynamic memory, resetting the object |
| [`load`](#load) | Load INI data from a file or string |
| [`has_option`](#has_option) | Inquire whether an option exists |
| [`has_section`](#has_section) | Inquire whether a section exists |
| [`section`](#section) | Get a section name by index |
| [`index`](#index) | Get the index of a named section or option |
| [`count_values`](#count_values) | Count the number of space-separated values in an option |
| [`add`](#add) | Add a section or option (updates value if option already exists) |
| [`get`](#get) | Get an option value |
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

Returns the name of the section at position `i`. Use with `Ns` to loop over all sections.

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: sec_name
integer                       :: s

call fini%load(filename='config.ini')

do s = 1, fini%Ns
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

s = fini%index(section='database')
if (s > 0) print *, 'database is section #', s

o = fini%index(section='database', option='host')
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

call fini%add(section='sec-foo')
call fini%add(section='sec-foo', option='bar',   val=-32.1_R8P)
call fini%add(section='sec-foo', option='baz',   val=' hello FiNeR! ')
call fini%add(section='sec-foo', option='array', val=[1, 2, 3, 4])
call fini%add(section='sec-bar')
call fini%add(section='sec-bar', option='bools', val=[.true., .false., .false.])
```

---

## `get` {#get}

Retrieves an option value. The receiving variable (`val`) can be a scalar or an array of any intrinsic type. The optional `delimiter` argument specifies the separator between array values (default: space).

```fortran
use finer
use penf, only: I4P
type(file_ini)       :: fini
integer(I4P)         :: error
integer, allocatable :: arr(:)
character(64)        :: host

call fini%load(filename='config.ini')

call fini%get(section_name='database', option_name='host',  val=host,  error=error)
call fini%get(section_name='foo',      option_name='array', val=arr,   error=error)
if (error == 0) print *, arr
```

::: tip
Always allocate the receiving array to the correct size with `count_values` before calling `get` with an array `val`.
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

**Signatures:**
```fortran
do while (fini%loop(option=opt))             ! all options in file
do while (fini%loop(section_name=, option=)) ! options in one section
```

```fortran
use finer
type(file_ini)                :: fini
character(len=:), allocatable :: opt(:)

call fini%load(filename='config.ini')

! Iterate over all options in 'database' section
do while (fini%loop(section_name='database', option=opt))
  print *, trim(opt(1)), ' = ', trim(opt(2))
end do

! Iterate over every option in the entire file
do while (fini%loop(option=opt))
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

call fini%add(section='sec-foo', option='bar', val=-32.1_R8P)
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
