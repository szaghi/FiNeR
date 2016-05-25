<a name="top"></a>

# FiNeR [![GitHub tag](https://img.shields.io/github/tag/szaghi/FiNeR.svg)]()

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/FiNeR.svg?branch=master)](https://travis-ci.org/szaghi/FiNeR)
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/FiNeR.svg)](http://codecov.io/github/szaghi/FiNeR?branch=master)

### FiNeR, Fortran INI ParseR and generator for FoRtraners

+ FiNeR is a pure Fortran (KISS) library for IO of INI (config) files for modern (2003+) Fortran projects;
+ FiNeR is Fortran 2003+ standard compliant;
- FiNeR is OOP designed;
- FiNeR is TDD designed;
+ FiNeR is a Free, Open Source Project.

#### Issues
[![GitHub issues](https://img.shields.io/github/issues/szaghi/FiNeR.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/FiNeR.png?label=ready&title=Ready)](https://waffle.io/szaghi/FiNeR)
[![In Progress](https://badge.waffle.io/szaghi/FiNeR.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/FiNeR)
[![Open bugs](https://badge.waffle.io/szaghi/FiNeR.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/FiNeR)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v5.3.0+-orange.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v16.x+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[What is FiNeR?](#what-is-finer?) | [Main features](#main-features) | [Copyrights](#copyrights) | [Documentation](#documentation) | [A Taste of FiNeR](#a-taste-of-finer)

---

## What is FiNeR?

Modern Fortran standards (2003+) have introduced better support for strings manipulations. Exploiting such new Fortran capabilities, FiNeR provides an easy to use module library for input (parsing) and output (generating) [INI](http://en.wikipedia.org/wiki/INI_file) (config) files.

Go to [Top](#top)

## Main features

* [X] User-friendly methods for IO INI files:
    * [x] parsing files:
        * [x] automatic parsing of all sections (whose number is auto-detected);
        * [x] automatic parsing of all options of each section (whose number is auto-detected);
        * [x] unlimited polymorphic option-values extraction;
    * [x] generating files:
        * [x] user-friendly add/remove sections/options;
        * [x] unlimited polymorphic option-values setting;
    * [x] introspection:
        * [x] self-consistency checks;
        * [x] pretty-printings;
        * [x] loop over options;
        * [x] inquiring the presence of sections/options by name;
* [ ] errors trapping mechanism.

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

FiNeR is an open source project, it is distributed under a multi-licensing system:

+ for FOSS projects:
  - [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html);
+ for closed source/commercial projects:
  - [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause);
  - [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause);
  - [MIT](http://opensource.org/licenses/MIT).

Anyone is interest to use, to develop or to contribute to FiNeR is welcome, feel free to select the license that best matches your soul!

More details can be found on [wiki](https://github.com/szaghi/FiNeR/wiki/Copyrights).

Go to [Top](#top)

## Documentation

Besides this README file the FiNeR documentation is contained into its own [wiki](https://github.com/szaghi/FiNeR/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/FiNeR/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

### A Taste of FiNeR
Let us assume our goal is to parse a config file. It is as simple as
```fortran
USE Lib_INI_IO
...
type(Type_File_INI)::           fini     ! INI File.
character(len=:), allocatable:: source   ! Testing string.
real(R4P), allocatable::        array(:) ! Array option.
integer(I4P)::                  error    ! Error code.
...
source='[section-1]'//new_line('A')//    &
       'option-1 = one'//new_line('A')// &
       'option-2 = 2.'//new_line('A')//  &
       '           3.'//new_line('A')//  &
       'option-3 = bar'//new_line('A')// &
       '[section-2]'//new_line('A')//    &
       'option-1 = foo'
call fini%load(source=source)
allocate(array(1:fini%count_values(section='section-1',option='option-2')))
call fini%get(section='section-1',option='option-2',val=array,error=error)
if (error==0) then
  print*,array
else
  ! errors occur...
endif
```
And what about the generation of an INI file? It is simple as parsing an old one:
```fortran
USE Lib_INI_IO
...
type(Type_File_INI):: fini ! INI File.
...
call fini%add(section='sec-foo')
call fini%add(section='sec-foo',option='bar',val=-32.1_R8P)
call fini%add(section='sec-foo',option='baz',val=' hello FiNeR! ')
call fini%add(section='sec-foo',option='array',val=[1,2,3,4])
call fini%add(section='sec-bar')
call fini%add(section='sec-bar',option='bools',val=[.true.,.false.,.false.])
call fini%save(filename='foo.ini')
```
A file named *foo.ini* is created. It contains something like:
```ini
[sec-foo]
bar = -0.321000000000000E+002
baz =  hello FiNeR!
array = +1 +2 +3 +4
[sec-bar]
bools = T F F
```

Go to [Top](#top)
