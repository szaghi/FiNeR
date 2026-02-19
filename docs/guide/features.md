---
title: Features
---

# Features

## INI Parsing

- Load INI data from a physical file or from an in-memory character string
- Automatic detection of all sections — no need to declare section names upfront
- Automatic detection of all options within each section
- Multi-line option values (continuation lines without `=` are joined to the previous option)
- Inline comments stripped automatically (delimited by `;`)
- Configurable option separator (default `=`, can be set to `:` or any single character)

## INI Generation

- Add sections and options programmatically with `add`
- Set option values of any intrinsic type (integer, real, logical, character) and all PENF kinds
- Array-valued options with optional delimiter
- Remove sections and options with `del`
- Save to a named file with `save`

## Introspection

- Check whether a section exists: `has_section`
- Check whether an option exists (globally or within a section): `has_option`
- Get a section name by index: `section`
- Get an option name by index: `option` (not yet in public API — use `loop`)
- Get the index of a named section or option: `index`
- Count the number of space-separated values in an option: `count_values`
- Retrieve all option name/value pairs as a 2-D character array: `items`
- Iterate over all options (or options of one section) with a `while` loop: `loop`
- Pretty-print the full file contents to any Fortran unit: `print`

## INI Format Details

FiNeR follows the common INI conventions:

| Element | Syntax |
|---------|--------|
| Section header | `[section-name]` |
| Option | `key = value` |
| Comment line | `!`, `;`, or `#` as first non-blank character |
| Inline comment | `;` followed by comment text |
| Multi-line value | continuation line has no `=`; it is appended to the previous option |
| Default separator | `=` (configurable) |

Example:

```ini
; database settings
[database]
host = localhost
port = 5432
tags = alpha beta gamma

[logging]
level = info
! end of file
```

## Compiler Support

| Compiler | Status |
|----------|--------|
| GNU gfortran ≥ 5.3 | Supported |
| Intel Fortran ≥ 16.x | Supported |
| IBM XL Fortran | Not tested |
| g95 | Not tested |
| NAG Fortran | Not tested |
| PGI / NVIDIA | Not tested |

## Design Principles

- **Pure Fortran** — no C extensions, no system calls beyond standard I/O
- **OOP** — all functionality exposed as type-bound procedures on `file_ini`
- **TDD** — every public method is exercised by automated tests in `src/tests/`
- **KISS** — simple, focused API without unnecessary abstractions
- **Free & Open Source** — multi-licensed for both FOSS and commercial use
