---
title: finer_option_t
---

# finer_option_t

> Option class definition.

**Source**: `src/lib/finer_option_t.F90`

**Dependencies**

```mermaid
graph LR
  finer_option_t["finer_option_t"] --> finer_backend["finer_backend"]
```

## Contents

- [option](#option)
- [integer_string](#integer-string)
- [real_string](#real-string)
- [option](#option)
- [free](#free)
- [get_pairs](#get-pairs)
- [get_string](#get-string)
- [parse](#parse)
- [get_option](#get-option)
- [get_a_option](#get-a-option)
- [parse_comment](#parse-comment)
- [parse_name](#parse-name)
- [parse_value](#parse-value)
- [print_option](#print-option)
- [set_option](#set-option)
- [set_a_option](#set-a-option)
- [save_option](#save-option)
- [assign_option](#assign-option)
- [assign_default](#assign-default)
- [convert](#convert)
- [split_complex](#split-complex)
- [count_values](#count-values)
- [name_len](#name-len)
- [values_len](#values-len)
- [option_eq_string](#option-eq-string)
- [option_eq_character](#option-eq-character)
- [integer_string_I8P](#integer-string-i8p)
- [integer_string_I4P](#integer-string-i4p)
- [integer_string_I2P](#integer-string-i2p)
- [integer_string_I1P](#integer-string-i1p)
- [is_complex](#is-complex)
- [is_complex_list](#is-complex-list)
- [real_string_R8P](#real-string-r8p)
- [real_string_R4P](#real-string-r4p)
- [new_option](#new-option)

## Derived Types

### option

Option data of sections.

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `oname` | type(string) |  | Option name. |
| `ovals` | type(string) |  | Option values. |
| `ocomm` | type(string) |  | Eventual option inline comment. |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `count_values` | pass(self) | Counting option value(s). |
| `free` | pass(self) | Free dynamic memory. |
| `get` |  | Get option value (scalar). |
| `get_pairs` | pass(self) | Return option name/values pairs. |
| `get_string` | pass(self) | Get option value as an allocatable string. |
| `name_len` | pass(self) | Return option name length. |
| `parse` | pass(self) | Parse option data. |
| `print` | pass(self) | Pretty print data. |
| `save` | pass(self) | Save data. |
| `set` |  | Set option value (scalar). |
| `values_len` | pass(self) | Return option values length. |
| `assignment(=)` |  | Assignment overloading. |
| `operator(==)` |  | Equal operator overloading. |
| `get_option` | pass(self) | Get option value (scalar). |
| `get_a_option` | pass(self) | Get option value (array). |
| `parse_comment` | pass(self) | Parse option inline comment. |
| `parse_name` | pass(self) | Parse option name. |
| `parse_value` | pass(self) | Parse option values. |
| `set_option` | pass(self) | Set option value (scalar). |
| `set_a_option` | pass(self) | Set option value (array). |
| `assign_option` | pass(lhs) | Assignment overloading. |
| `option_eq_string` | pass(lhs) | Equal to string logical operator. |
| `option_eq_character` | pass(lhs) | Equal to character logical operator. |

## Interfaces

### integer_string

Return the string representing an integer number, without the plus sign of positive numbers.

**Module procedures**: [`integer_string_I8P`](/api/src/lib/finer_option_t#integer-string-i8p), [`integer_string_I4P`](/api/src/lib/finer_option_t#integer-string-i4p), [`integer_string_I2P`](/api/src/lib/finer_option_t#integer-string-i2p), [`integer_string_I1P`](/api/src/lib/finer_option_t#integer-string-i1p)

### real_string

Return the shortest string representing a real number that is read back exactly.

**Module procedures**: [`real_string_R8P`](/api/src/lib/finer_option_t#real-string-r8p), [`real_string_R4P`](/api/src/lib/finer_option_t#real-string-r4p)

### option

Overload `option` name with a function returning a new (initiliazed) option instance.

**Module procedures**: [`new_option`](/api/src/lib/finer_option_t#new-option)

## Subroutines

### free

Free dynamic memory.

**Attributes**: elemental

```fortran
subroutine free(self)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |

### get_pairs

Return option name/values pairs.

**Attributes**: pure

```fortran
subroutine get_pairs(self, pairs)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `pairs` | character(len=:) | out | allocatable | Option name/values pairs. |

**Call graph**

```mermaid
flowchart TD
  loop["loop"] --> get_pairs["get_pairs"]
  option_pairs["option_pairs"] --> get_pairs["get_pairs"]
  style get_pairs fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### get_string

Get option value as an allocatable string.

 `val` is (re)allocated with the length of the option value. If the option has no value, `val` is left unchanged and an
 error is returned.

```fortran
subroutine get_string(self, val, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `val` | character(len=:) | inout | allocatable | Value. |
| `error` | integer(kind=I4P) | out | optional | Error code. |

**Call graph**

```mermaid
flowchart TD
  get_string["get_string"] --> get_string["get_string"]
  get_string["get_string"] --> get_string["get_string"]
  style get_string fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### parse

Parse option data from a source string.

**Attributes**: elemental

```fortran
subroutine parse(self, sep, source, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |
| `sep` | character(len=*) | in |  | Separator of option name/value. |
| `source` | type(string) | inout |  | String containing option data. |
| `error` | integer(kind=I4P) | out |  | Error code. |

**Call graph**

```mermaid
flowchart TD
  load["load"] --> parse["parse"]
  parse["parse"] --> parse["parse"]
  parse_options["parse_options"] --> parse["parse"]
  parse["parse"] --> parse_comment["parse_comment"]
  parse["parse"] --> parse_name["parse_name"]
  parse["parse"] --> parse_value["parse_value"]
  style parse fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### get_option

for getting option data value (scalar).

 If the option value cannot be converted to the type of `val`, `val` is left unchanged and an error is returned.

```fortran
subroutine get_option(self, val, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `val` | class(*) | inout |  | Value. |
| `error` | integer(kind=I4P) | out | optional | Error code. |

**Call graph**

```mermaid
flowchart TD
  get_option["get_option"] --> convert["convert"]
  style get_option fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### get_a_option

Get option data values (array).

 If `val` cannot hold all the values, or a value cannot be converted to the type of `val`, `val` is left unchanged and
 an error is returned.

```fortran
subroutine get_a_option(self, val, delimiter, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `val` | class(*) | inout |  | Value. |
| `delimiter` | character(len=*) | in | optional | Delimiter used for separating values. |
| `error` | integer(kind=I4P) | out | optional | Error code. |

**Call graph**

```mermaid
flowchart TD
  get_a_option["get_a_option"] --> convert["convert"]
  get_a_option["get_a_option"] --> is_complex["is_complex"]
  get_a_option["get_a_option"] --> split_complex["split_complex"]
  style get_a_option fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### parse_comment

Parse option inline comment trimming it out from pure value string.

**Attributes**: elemental

```fortran
subroutine parse_comment(self)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |

**Call graph**

```mermaid
flowchart TD
  parse["parse"] --> parse_comment["parse_comment"]
  style parse_comment fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### parse_name

Parse option name from a source string.

**Attributes**: elemental

```fortran
subroutine parse_name(self, sep, source, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |
| `sep` | character(len=*) | in |  | Separator of option name/value. |
| `source` | type(string) | in |  | String containing option data. |
| `error` | integer(kind=I4P) | out |  | Error code. |

**Call graph**

```mermaid
flowchart TD
  parse["parse"] --> parse_name["parse_name"]
  parse["parse"] --> parse_name["parse_name"]
  style parse_name fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### parse_value

Parse option value from a source string.

**Attributes**: elemental

```fortran
subroutine parse_value(self, sep, source, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |
| `sep` | character(len=*) | in |  | Separator of option name/value. |
| `source` | type(string) | in |  | String containing option data. |
| `error` | integer(kind=I4P) | out |  | Error code. |

**Call graph**

```mermaid
flowchart TD
  parse["parse"] --> parse_value["parse_value"]
  style parse_value fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### print_option

Print data with a pretty format.

```fortran
subroutine print_option(self, unit, retain_comments, pref, iostat, iomsg)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `unit` | integer(kind=I4P) | in |  | Logic unit. |
| `retain_comments` | logical | in |  | Flag for retaining eventual comments. |
| `pref` | character(len=*) | in | optional | Prefixing string. |
| `iostat` | integer(kind=I4P) | out | optional | IO error. |
| `iomsg` | character(len=*) | out | optional | IO error message. |

### set_option

Set option data value (scalar).

 If the type of `val` is not supported the option value is left unchanged and an error is returned.

**Attributes**: pure

```fortran
subroutine set_option(self, val, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |
| `val` | class(*) | in |  | Value. |
| `error` | integer(kind=I4P) | out | optional | Error code. |

**Call graph**

```mermaid
flowchart TD
  set_option["set_option"] --> integer_string["integer_string"]
  set_option["set_option"] --> real_string["real_string"]
  style set_option fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### set_a_option

Set option data value (array).

 If the type of `val` is not supported the option value is left unchanged and an error is returned.

**Attributes**: pure

```fortran
subroutine set_a_option(self, val, delimiter, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Option data. |
| `val` | class(*) | in |  | Value. |
| `delimiter` | character(len=*) | in | optional | Delimiter used for separating values. |
| `error` | integer(kind=I4P) | out | optional | Error code. |

**Call graph**

```mermaid
flowchart TD
  set_a_option["set_a_option"] --> integer_string["integer_string"]
  set_a_option["set_a_option"] --> real_string["real_string"]
  style set_a_option fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### save_option

Save data.

```fortran
subroutine save_option(self, unit, retain_comments, iostat, iomsg)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `unit` | integer(kind=I4P) | in |  | Logic unit. |
| `retain_comments` | logical | in |  | Flag for retaining eventual comments. |
| `iostat` | integer(kind=I4P) | out | optional | IO error. |
| `iomsg` | character(len=*) | out | optional | IO error message. |

### assign_option

Assignment between two options.

**Attributes**: elemental

```fortran
subroutine assign_option(lhs, rhs)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `lhs` | class([option](/api/src/lib/finer_option_t#option)) | inout |  | Left hand side. |
| `rhs` | type([option](/api/src/lib/finer_option_t#option)) | in |  | Rigth hand side. |

### assign_default

Assign a default value to a value of one of the supported types.

 A complex `val` accepts a complex, real or integer default of any supported kind.
 A real `val` accepts a real or integer default of any supported kind, an integer `val` accepts an integer default of any
 supported kind (that it can represent), a logical or character `val` accepts a default of the same type. For any other
 combination `val` is left unchanged and an error is returned.

```fortran
subroutine assign_default(val, default, error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `val` | class(*) | inout |  | Value. |
| `default` | class(*) | in |  | Default value. |
| `error` | integer(kind=I4P) | out |  | Error code. |

**Call graph**

```mermaid
flowchart TD
  get_a_option["get_a_option"] --> assign_default["assign_default"]
  get_option["get_option"] --> assign_default["assign_default"]
  style assign_default fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### convert

Convert a string into a value of one of the supported types.

 If the string cannot be converted, or the type of `val` is not supported, `val` is left unchanged and an error is returned.

```fortran
subroutine convert(source, val, error, check_only)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `source` | character(len=*) | in |  | String to be converted. |
| `val` | class(*) | inout |  | Value. |
| `error` | integer(kind=I4P) | out |  | Error code. |
| `check_only` | logical | in | optional | Check the conversion without modifying `val`. |

**Call graph**

```mermaid
flowchart TD
  get_a_option["get_a_option"] --> convert["convert"]
  get_option["get_option"] --> convert["convert"]
  convert["convert"] --> is_numeric["is_numeric"]
  convert["convert"] --> scan_integer["scan_integer"]
  convert["convert"] --> split_parts["split_parts"]
  style convert fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### split_complex

Split a list of complex values in the Fortran notation, e.g. `(1.0, 2.0) (3.0, 4.0)`, into its values.

 Whatever is between the values is ignored: the delimiter can be also inside the values.

**Attributes**: pure

```fortran
subroutine split_complex(source, tokens)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `source` | character(len=*) | in |  | String to be split. |
| `tokens` | type(string) | out | allocatable | Complex values. |

**Call graph**

```mermaid
flowchart TD
  get_a_option["get_a_option"] --> split_complex["split_complex"]
  style split_complex fill:#3e63dd,stroke:#99b,stroke-width:2px
```

## Functions

### count_values

Get the number of values of option data.

**Attributes**: elemental

**Returns**: `integer(kind=I4P)`

```fortran
function count_values(self, delimiter) result(Nv)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |
| `delimiter` | character(len=*) | in | optional | Delimiter used for separating values. |

**Call graph**

```mermaid
flowchart TD
  count_values["count_values"] --> count_values["count_values"]
  count_values["count_values"] --> count_values["count_values"]
  file_ini_autotest["file_ini_autotest"] --> count_values["count_values"]
  count_values["count_values"] --> is_complex_list["is_complex_list"]
  style count_values fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### name_len

Return option name length.

**Attributes**: elemental

**Returns**: `integer`

```fortran
function name_len(self) result(length)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |

**Call graph**

```mermaid
flowchart TD
  max_chars_len["max_chars_len"] --> name_len["name_len"]
  style name_len fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### values_len

Return option values length.

**Attributes**: elemental

**Returns**: `integer`

```fortran
function values_len(self) result(length)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Option data. |

**Call graph**

```mermaid
flowchart TD
  max_chars_len["max_chars_len"] --> values_len["values_len"]
  style values_len fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### option_eq_string

Equal to string logical operator.

**Attributes**: elemental

**Returns**: `logical`

```fortran
function option_eq_string(lhs, rhs) result(is_it)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `lhs` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Left hand side. |
| `rhs` | type(string) | in |  | Right hand side. |

### option_eq_character

Equal to character logical operator.

**Attributes**: elemental

**Returns**: `logical`

```fortran
function option_eq_character(lhs, rhs) result(is_it)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `lhs` | class([option](/api/src/lib/finer_option_t#option)) | in |  | Left hand side. |
| `rhs` | character(kind=CK, len=*) | in |  | Right hand side. |

### integer_string_I8P

Return the string representing an integer number (I8P), without the plus sign of positive numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function integer_string_I8P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | integer(kind=I8P) | in |  | Number. |

### integer_string_I4P

Return the string representing an integer number (I4P), without the plus sign of positive numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function integer_string_I4P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | integer(kind=I4P) | in |  | Number. |

### integer_string_I2P

Return the string representing an integer number (I2P), without the plus sign of positive numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function integer_string_I2P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | integer(kind=I2P) | in |  | Number. |

### integer_string_I1P

Return the string representing an integer number (I1P), without the plus sign of positive numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function integer_string_I1P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | integer(kind=I1P) | in |  | Number. |

### is_complex

Return true if the values are of complex type.

**Attributes**: pure

**Returns**: `logical`

```fortran
function is_complex(val)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `val` | class(*) | in |  | Values. |

**Call graph**

```mermaid
flowchart TD
  get_a_option["get_a_option"] --> is_complex["is_complex"]
  style is_complex fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### is_complex_list

Return true if the string is a list of complex values in the Fortran notation, e.g. `(1.0, 2.0) (3.0, 4.0)`.

**Attributes**: pure

**Returns**: `logical`

```fortran
function is_complex_list(source)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `source` | character(len=*) | in |  | String to be checked. |

**Call graph**

```mermaid
flowchart TD
  count_values["count_values"] --> is_complex_list["is_complex_list"]
  style is_complex_list fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### real_string_R8P

Return the shortest string representing a real number (R8P) that is read back exactly.

 It is the compact string of PENF without the plus sign of the not negative numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function real_string_R8P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | real(kind=R8P) | in |  | Number. |

### real_string_R4P

Return the shortest string representing a real number (R4P) that is read back exactly.

 It is the compact string of PENF without the plus sign of the not negative numbers.

**Attributes**: pure

**Returns**: `character(len=:)`

```fortran
function real_string_R4P(n) result(string)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `n` | real(kind=R4P) | in |  | Number. |

### new_option

Return a new (initiliazed) option instance.

**Attributes**: elemental

**Returns**: type([option](/api/src/lib/finer_option_t#option))

```fortran
function new_option(option_name, option_values, option_comment)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `option_name` | character(len=*) | in | optional | Option name. |
| `option_values` | character(len=*) | in | optional | Option values. |
| `option_comment` | character(len=*) | in | optional | Option comment. |
