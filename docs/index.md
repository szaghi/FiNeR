---
layout: home

hero:
  name: FiNeR
  text: Fortran INI ParseR
  tagline: Read, edit and write INI configuration files from modern Fortran, with typed values and no surprises.
  image:
    light: /hero.svg
    dark: /hero-dark.svg
    alt: FiNeR mark, a section bracket brushed in indigo
  actions:
    - theme: brand
      text: Start the tutorial
      link: /manual/
    - theme: alt
      text: Cookbook
      link: /manual/cookbook
    - theme: alt
      text: Reference
      link: /guide/features
    - theme: alt
      text: GitHub
      link: https://github.com/szaghi/FiNeR

features:
  - icon:
      light: /icons/parsing.svg
      dark: /icons/parsing-dark.svg
      width: 48
      height: 48
    title: Load files and strings
    details: Sections, the global section, three comment styles, inline comments, values on several lines, a custom separator, Windows line endings.
    link: /guide/syntax
  - icon:
      light: /icons/get.svg
      dark: /icons/get-dark.svg
      width: 48
      height: 48
    title: Typed values
    details: One get for integers, reals (quad precision included), complex numbers, logicals and strings, scalars or lists, with defaults.
    link: /guide/reading
  - icon:
      light: /icons/generation.svg
      dark: /icons/generation-dark.svg
      width: 48
      height: 48
    title: Edit and write
    details: One add to create sections and set options of any type; reals written as the shortest text that reads back exactly.
    link: /guide/writing
  - icon:
      light: /icons/introspection.svg
      dark: /icons/introspection-dark.svg
      width: 48
      height: 48
    title: Explore any file
    details: Sections by number or name, options by name, which section holds an option, positions, counts, loops, every pair at once.
    link: /guide/exploring
  - icon:
      light: /icons/semi.svg
      dark: /icons/semi-dark.svg
      width: 48
      height: 48
    title: Failures reported, never fatal
    details: An error or iostat on every call that can fail; a failed get leaves its value untouched, arrays are checked whole.
    link: /guide/errors
  - icon:
      light: /icons/build.svg
      dark: /icons/build-dark.svg
      width: 48
      height: 48
    title: FoBiS, CMake or fpm
    details: Build it the way your project does; pure Fortran 2003+, tested with gfortran and nvfortran. GPL v3, or BSD and MIT for any use.
    link: /guide/installation
---

## A tour

`ini`, a command line tool of about 50 lines built on FiNeR
([`docs/examples/src/tour.f90`](https://github.com/szaghi/FiNeR/blob/master/docs/examples/src/tour.f90)), lists the
sections of a file, reads a value, changes it and saves the file:

<div class="finer-casts">
  <img src="./examples/images/tour.svg" alt="An animated terminal session: ini lists the sections of case.ini, reads time.dt, sets it to 5e-5, reads it again, and reports an option that is missing">
</div>

## Quick start

<<< @/examples/snippets/heat_2-load.f90

<<< @/examples/snippets/heat_2-typed.f90

The [tutorial](/manual/) builds a complete program from here, chapter by chapter; the [cookbook](/manual/cookbook) has
a recipe for each everyday task.
