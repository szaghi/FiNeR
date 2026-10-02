# Changelog

All notable changes to this project are documented here.
Versions follow [Semantic Versioning](https://semver.org/).
Format follows [Keep a Changelog](https://keepachangelog.com/).

## [2.2.0] — 2026-10-02
### Changed
- **option**: Delegate real formatting to PENF compact str


## [2.1.0] — 2026-10-02
### Added
- **get**: Add get_string and default values for get

- **parser**: Keep options before the first section in a global section

- **option**: Support complex values in get, add and default


### Documentation
- **usage**: Document subsections as dotted section names


### Fixed
- **docs**: Untrack package-lock and pin esbuild for lock-free vite build

- **parser**: Report silent failures in parsing, get and add

- **install**: Document dependency fetch and run ctest from project root

- **option**: Report values that cannot be converted in get

- **build**: Exclude dependency docs and scripts from fobis builds

- **coverage**: Restrict coverage to src/lib

- **option**: Write reals as the shortest text that reads back exactly

- **parser**: Merge '[]' into the global section, drop '+' on integers

- **build**: Enable quad precision under cmake, repair makedoc rule

- **loop**: Keep loop state in the object, fix assignment, add tests ⚠ BREAKING CHANGE


## [2.0.10] — 2026-05-10
### Fixed
- **fobos**: Correct gcov-analyzer flag syntax in makecoverage-analysis rule

- **build**: Resolve parallel-build race with stringifor


### Security
- **scaffold**: Sync boilerplate from FoBiS 3.8.11


## [2.0.9] — 2026-03-02
### Fixed
- **install**: Skip fpm gracefully when fpm.toml is absent


## [2.0.8] — 2026-03-02
### Added
- **install**: Add fpm build support with fpm.toml guard


## [2.0.5] — 2026-02-19
### Documentation
- Add CLAUDE.md and update submodules

- Add VitePress site, rewrite README, and add release pipeline



