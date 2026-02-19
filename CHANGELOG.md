# Changelog
## [Unreleased]
### Documentation
- Add CLAUDE.md and update submodules ([`ad63ea6`](https://github.com/szaghi/FLAP/commit/ad63ea6b45dabc0923092149a766c076f9591834))

### Miscellaneous
- Update fortran tester submodule ([`b57eaba`](https://github.com/szaghi/FLAP/commit/b57eabaaf9052faf87a4821ff92c99bedf231827))
- Update makecoverage ([`fa67aaa`](https://github.com/szaghi/FLAP/commit/fa67aaaee59d2c5953a0ea942b51b9de559fdc7a))
- Remove make test from GH actions ([`1039836`](https://github.com/szaghi/FLAP/commit/1039836403ecfd3097e65446a5d4d615393b5b54))
- Merge remote-tracking branch 'refs/remotes/origin/master' ([`aab8f72`](https://github.com/szaghi/FLAP/commit/aab8f72d97764d0c0c850653be8fc78df58072b5))

## [v2.0.4](https://github.com/szaghi/FLAP/tree/v2.0.4) (2022-07-05)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v2.0.3...v2.0.4)
### Bug fixes
- Fix merge ([`710da0b`](https://github.com/szaghi/FLAP/commit/710da0b9ee828359d5047323030bfc87d343b1a7))

### Miscellaneous
- Make public filename member

Make public filename member

Why:

More easy file handling.*

Side effects:

Nothing. ([`0295418`](https://github.com/szaghi/FLAP/commit/02954183107f4ef2014e3efbfa7de2797706c446))
- Update submodules ([`9acaba9`](https://github.com/szaghi/FLAP/commit/9acaba98a71f98e9294bbb094355903161a722b0))
- Update stringifor submodule ([`ab9e3af`](https://github.com/szaghi/FLAP/commit/ab9e3afd3228345ba4ed5cb557db7aad25c9134d))
- Temporary fix for building FiNeR library with intel compilers

Temporary fix for building FiNeR library with intel compilers: Intel
2019 has serious bug and works differently from intel 2018 and from GNU
Fortran... ([`0a84981`](https://github.com/szaghi/FLAP/commit/0a8498100a73866558ba43c5082d42777b042568))
- Update submodules ([`68109c1`](https://github.com/szaghi/FLAP/commit/68109c107283662e6eaecb60aaf8ee1405f6246f))
- Update travis config ([`ace208d`](https://github.com/szaghi/FLAP/commit/ace208d93c963a08435a78bf29ea5657f011e431))
- Update travis config ([`30858fc`](https://github.com/szaghi/FLAP/commit/30858fc155d425c284d853959edd298917135e5d))
- Update third party submodules ([`005a9a5`](https://github.com/szaghi/FLAP/commit/005a9a533ed8158bf646f42adc3f6058d3b7293d))
- Update submodules ([`f587081`](https://github.com/szaghi/FLAP/commit/f587081a9d9c531d727c09dff6d9cde13dae20d2))
- Update submodules ([`d61b551`](https://github.com/szaghi/FLAP/commit/d61b551fe89a01189735fad46c95efe35f356795))
- Add cmake support ([`395df9d`](https://github.com/szaghi/FLAP/commit/395df9d9f68dcea2384e7ea6379b30fd9495dc94))
- Add comment, fix typo ([`6f491a2`](https://github.com/szaghi/FLAP/commit/6f491a2fcf6e1955ab33dc42dcc418fd63fa3fb1))
- Update minimal cmake required ([`b30ad8a`](https://github.com/szaghi/FLAP/commit/b30ad8a7edcc187710e6cc0d667fd9d143d5c0b9))
- Update ([`8a9e921`](https://github.com/szaghi/FLAP/commit/8a9e92134fbba7f5fe363101b70f3e4935ca57b2))
- Update ([`fdc4298`](https://github.com/szaghi/FLAP/commit/fdc4298f17748f0a50ac14ef5a918c1ad735419e))
- Update ([`3a4c393`](https://github.com/szaghi/FLAP/commit/3a4c3930f01ff1301a62e8ae6d1a2d8838ca3e9b))
- Merge ([`0fe02cc`](https://github.com/szaghi/FLAP/commit/0fe02ccd1732479e4b5eb9dffbaea78de4369277))
- Update submodules ([`4e5ab99`](https://github.com/szaghi/FLAP/commit/4e5ab9967db809cb428441ffeadde4b1fc6e24c4))
- Update ([`91c91a3`](https://github.com/szaghi/FLAP/commit/91c91a3d99d0d333983d1b2b5d32a67ce7a11c03))
- Update ([`1e7e311`](https://github.com/szaghi/FLAP/commit/1e7e311d5053c7045f68784c8687fa5067517a08))
- Update ([`9986732`](https://github.com/szaghi/FLAP/commit/99867320c99616ab39d2d131991e1525404bf89c))
- Merge pull request [#13](https://github.com/szaghi/FLAP/issues/13) from kostyfisik/cmake

add cmake support ([`86dc016`](https://github.com/szaghi/FLAP/commit/86dc016db6eee17771d4607817ddb4fd9c79f7be))
- Merge branch 'master' of github.com:szaghi/FiNeR ([`aa3292e`](https://github.com/szaghi/FLAP/commit/aa3292e8a197e68aeb3ddbcaf109957bbd0385f7))
- Update submodules ([`ff900a2`](https://github.com/szaghi/FLAP/commit/ff900a213c19167e59cb9eedfd93a7e4203f31cf))
- Update readme example, fix #issue16

Examples into readme still refer to old version of the library, updated
to the new API.

Why:

This drives new users to confusion, the readme is read before (and often
the only to be read) tests and other documentation resources.

Travis setting updated also. ([`9e46944`](https://github.com/szaghi/FLAP/commit/9e46944ebb0cc2c91d8a8fd250c55c3410723d23))
- Update parse test for #issue16

Short description

Update parse test for issue 16: it seems that all comments markers work
as expected, needed more details from issue author. ([`8c747c6`](https://github.com/szaghi/FLAP/commit/8c747c61cde0424fbf5e58e74282da440d13f730))
- Re-add pre-processing flag for unsupported R16P ([`280b2c3`](https://github.com/szaghi/FLAP/commit/280b2c3fb9917e37ec233122386d70765de92e87))
- Enable library to be used with NVFortran compiler on GPU-based architecture ([`847501d`](https://github.com/szaghi/FLAP/commit/847501df0632c03e02c5402bfac8f042112892d3))
- Add error checking for failing parsing takens ([`2b04def`](https://github.com/szaghi/FLAP/commit/2b04def2f874d0a401defac456f47c666a59a3fe))
- Switch to GH actions ([`5c391b2`](https://github.com/szaghi/FLAP/commit/5c391b29dd32e621839bac699bb11f0a1379a9b0))

## [v2.0.3](https://github.com/szaghi/FLAP/tree/v2.0.3) (2017-06-29)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v2.0.2...v2.0.3)
### Bug fixes
- Fix bug in load method ([`383257b`](https://github.com/szaghi/FLAP/commit/383257b7f0e5680012d7d13fb1be42c8ff9aa370))

### Miscellaneous
- Merge tag 'v2.0.2' into develop

Fix bug[#12](https://github.com/szaghi/FLAP/issues/12)

Stable release, fully backward compatible. ([`ca12c6c`](https://github.com/szaghi/FLAP/commit/ca12c6cacaa7c3cd9cb74235151dc395929246ae))
- Sanitize preprocessing flags ([`daff263`](https://github.com/szaghi/FLAP/commit/daff263460c33d9562df5c3414082f00cb86e563))
- Revert back to autotest ([`daa39df`](https://github.com/szaghi/FLAP/commit/daa39dff53b60c680359d3d1045146cd6abd2931))

## [v2.0.2](https://github.com/szaghi/FLAP/tree/v2.0.2) (2017-06-29)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v2.0.1...v2.0.2)
### Bug fixes
- Fix varius bugs... ([`585eab4`](https://github.com/szaghi/FLAP/commit/585eab4a46ff57896e6d9a9aea23bdb043e97cc5))
- Fix get error status ([`190b2cf`](https://github.com/szaghi/FLAP/commit/190b2cfd65be89a11685b724fda9d57907d65015))
- Fix iostat of file read ([`9822d84`](https://github.com/szaghi/FLAP/commit/9822d848ab68139189f2aaf878cd7a944ed784d1))
- Fix iostat check on read file ([`db0f285`](https://github.com/szaghi/FLAP/commit/db0f28561f1b81b64e9638a1b95defa329a01301))
- Fix FACE submodule lack ([`7d39b12`](https://github.com/szaghi/FLAP/commit/7d39b128e428389a21bbac0a8b8f0bb5af50b25c))
- Fix coverage issue ([`99e23c1`](https://github.com/szaghi/FLAP/commit/99e23c12aa6d66b32160d54955c15279f8455274))
- Fix typos in fobos that prevent to build FiNeR as standalone library ([`1ef764c`](https://github.com/szaghi/FLAP/commit/1ef764c14a7d7f32f06392f5f5273881287edf6c))

### Miscellaneous
- Merge tag 'v2.0.1' into develop

Add sections list method, stable release, fully backward compatible. ([`c16a557`](https://github.com/szaghi/FLAP/commit/c16a5575d8a9efb8ea52aa04cd3e60d58c8ed76b))
- Update to last PENF ([`f560648`](https://github.com/szaghi/FLAP/commit/f56064858fc8178421ce714b1dd691d6df1479c7))
- Update to last StringiFor ([`7147d7f`](https://github.com/szaghi/FLAP/commit/7147d7fcbd072c0bfcd6571224a6c570370c087e))
- Update travis config ([`3ac682e`](https://github.com/szaghi/FLAP/commit/3ac682e7bc98ca7b9252d64296ffefdfd3f24438))
- Update third parties ([`35f3c69`](https://github.com/szaghi/FLAP/commit/35f3c691269c6a2aa1eba9d1b075c7d76411796b))
- Merge branch 'master' into develop ([`980d233`](https://github.com/szaghi/FLAP/commit/980d2338f56c48b1d9826d8658ba4b31535cdcf9))
- Add new load test and fix error in stringifor%split ([`b755be1`](https://github.com/szaghi/FLAP/commit/b755be1f1d2e1dcaf2a1d2ea7906b5852309ab22))
- Update stringifor submodule ([`8512dbd`](https://github.com/szaghi/FLAP/commit/8512dbdb2f3b3f0a028b0e6a95d118bdc72d4ad5))
- Merge branch 'develop' ([`1f44b0f`](https://github.com/szaghi/FLAP/commit/1f44b0f1333d5ed25f3b31f751d0af35b380656c))
- Update StringiFor submodule ([`aaa017d`](https://github.com/szaghi/FLAP/commit/aaa017da7ee7887dbb65bd413a8e7a1635a50a9b))
- Trim out dangerous recursive git clone/update ([`3247824`](https://github.com/szaghi/FLAP/commit/3247824e757420b45a28e819dcf59dda7d81a566))
- Update submodule ([`277a548`](https://github.com/szaghi/FLAP/commit/277a5486773261c25fbdff03d828aef783acab8b))
- Update doc ([`76f1f7a`](https://github.com/szaghi/FLAP/commit/76f1f7a915f84469848911bbd854d009b8328825))
- Update fobos ([`9b6a0c6`](https://github.com/szaghi/FLAP/commit/9b6a0c63755bdea612c645da47556da7d1f21eb2))
- Update submodules ([`d8c3892`](https://github.com/szaghi/FLAP/commit/d8c3892bd15c81a2f2a5e0c361ab960215511096))
- Add test for add-update option, there is a bug... ([`5de35d4`](https://github.com/szaghi/FLAP/commit/5de35d41d2ffa9cd6501e713562faf7a33069019))
- Update submodule and fix double definition of CK

Update submodule and fix double definition of CK

Why:

CK was defined in both PENF and StringiFor, la last one is disabled.

This change addresses the need by:

Add "only" to stringifor usage.

Side effects:

Nothing. ([`071b26f`](https://github.com/szaghi/FLAP/commit/071b26f6a74dd937bf766906ac88105ae3bdd42d))
- Merge branch 'release/2.0.2' ([`f8dca3a`](https://github.com/szaghi/FLAP/commit/f8dca3a93d1222169132ef360bdc94ceeae9aca0))
- Update submodules ([`07307e2`](https://github.com/szaghi/FLAP/commit/07307e2646ae4b057ab1d209286e0a14b21691ed))
- Merge branch 'hotfix/fix-broken-library-building-issue[#11](https://github.com/szaghi/FLAP/issues/11)' ([`4ab2d1c`](https://github.com/szaghi/FLAP/commit/4ab2d1c2377edf051fddf14acbee8d8bcb7c3545))
- Merge tag 'vfix-broken-library-building-issue[#11](https://github.com/szaghi/FLAP/issues/11)' into develop

Fix issue[#11](https://github.com/szaghi/FLAP/issues/11)

Stable release, fully backward compatible. ([`e7cdce0`](https://github.com/szaghi/FLAP/commit/e7cdce032f2dce34872f03c282e7e25d6ee11b24))
- Merge branch 'develop' of github.com:szaghi/FiNeR into develop ([`97929e3`](https://github.com/szaghi/FLAP/commit/97929e35aa11741425a3d0a006908f9d503f6c63))
- Fix issue[#12](https://github.com/szaghi/FLAP/issues/12)

Fix bug on add-update option.

Stable release, fully backward compatible. ([`8861d69`](https://github.com/szaghi/FLAP/commit/8861d69c4694b0525d4c6bd2e791653cbdc1099c))
- Fix bug[#12](https://github.com/szaghi/FLAP/issues/12) on add-update option

Fix bug[#12](https://github.com/szaghi/FLAP/issues/12) on add-update option.

Sanitize Travis deployment.

Stable release, fully backward compatible. ([`2946050`](https://github.com/szaghi/FLAP/commit/29460508c9c931966e661d9ea83f38bce1c93d29))
- Merge branch 'release/2.0.2' ([`d0dd67a`](https://github.com/szaghi/FLAP/commit/d0dd67ad5d7213357fda64d336e9ba5d55983ce8))

## [v2.0.1](https://github.com/szaghi/FLAP/tree/v2.0.1) (2016-05-25)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v2.0.0...v2.0.1)
### Bug fixes
- Fix doc ([`b17454f`](https://github.com/szaghi/FLAP/commit/b17454f5eec6ab103cc39b85ac600cea062145a4))

### Miscellaneous
- Merge tag 'v2.0.0' into develop

Code refactored, stable release, not fully backward compatible. ([`5cfd23c`](https://github.com/szaghi/FLAP/commit/5cfd23c02a8e9c28bfd2218bd2cf2b6f2c870e4b))
- Try to fix travis issue ([`d21cc3a`](https://github.com/szaghi/FLAP/commit/d21cc3a52733e7a0e6b6c994a801cab5603a5894))
- Try to fix travis issue ([`94ea981`](https://github.com/szaghi/FLAP/commit/94ea9813e77beedb59770d71be77b96949d5cdcd))
- Add licensing system ([`0dd17df`](https://github.com/szaghi/FLAP/commit/0dd17dfea1042ebd1e678412d9b174b1689fd6c9))
- Add codecov config ([`f07bbe0`](https://github.com/szaghi/FLAP/commit/f07bbe0dec5bf7a6449eff0da7f9a33348e479d3))
- Play with codecov config ([`f6af3bd`](https://github.com/szaghi/FLAP/commit/f6af3bdbe9408d78980c4ed604ecd5b486d06b37))
- Merge branch 'master' into develop ([`43f231f`](https://github.com/szaghi/FLAP/commit/43f231fb36e5405b101babc0b06b961e04a2d740))
- Rename test ([`7e48e11`](https://github.com/szaghi/FLAP/commit/7e48e115b4df6595d938bb133e37e7fe1fe9ab32))
- Merge branch 'release/2.0.1' ([`05fb041`](https://github.com/szaghi/FLAP/commit/05fb041545ef6aab06e7d85ce44fe0ecde4130a9))

## [v2.0.0](https://github.com/szaghi/FLAP/tree/v2.0.0) (2016-05-24)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v1.0.2...v2.0.0)
### Miscellaneous
- Freeze before major refactor by StringiFor ([`fd59535`](https://github.com/szaghi/FLAP/commit/fd59535eb50f91c938655633d6c5f90b03089bd1))
- Change tree structure ([`51a06cb`](https://github.com/szaghi/FLAP/commit/51a06cb549d6e5f13bf03fac7277aedd18ca600b))
- Add PENF submodule: compile with only Intel... ([`2b92810`](https://github.com/szaghi/FLAP/commit/2b928101cf95e1f948a1652dcf91d62f9ae24a69))
- Modify sources organization ([`10c895b`](https://github.com/szaghi/FLAP/commit/10c895b3e33f2c8b3cb1c3286ff46fba308b788a))
- Remove old modules ([`3ec429f`](https://github.com/szaghi/FLAP/commit/3ec429f4666aa80efaa7940a11116bad198c9e57))
- Complete StringiFor adoption ([`3a862f0`](https://github.com/szaghi/FLAP/commit/3a862f0e79f0a51bdbd27b25a2e91812d9ac82c3))
- Update to new stringifor version

Update to new stringifor version ([`60c5b45`](https://github.com/szaghi/FLAP/commit/60c5b45512eea4d3a1905badc533bf4a8f49694d))
- Add travis support ([`ac88ce4`](https://github.com/szaghi/FLAP/commit/ac88ce4ecbf1c15fde414b557be8086cc5ed589e))
- Merge branch 'release/2.0.0' ([`cc2d451`](https://github.com/szaghi/FLAP/commit/cc2d451cbf803ef9be98ce3275ba67d5e40ffcf3))

## [v1.0.2](https://github.com/szaghi/FLAP/tree/v1.0.2) (2015-04-01)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v1.0.1...v1.0.2)
### Miscellaneous
- Improve public API with many new methods ([`0409377`](https://github.com/szaghi/FLAP/commit/0409377dd9efcf322195e3faa67234f57bdb326b))

## [v1.0.1](https://github.com/szaghi/FLAP/tree/v1.0.1) (2015-03-31)
[Full Changelog](https://github.com/szaghi/FLAP/compare/v1.0.0...v1.0.1)
### Miscellaneous
- Many changes, see Changelog of wiki ([`6f5ff2b`](https://github.com/szaghi/FLAP/commit/6f5ff2b3e558904a934ca5c3caac24093d6f2891))

## [v1.0.0](https://github.com/szaghi/FLAP/tree/v1.0.0) (2015-03-25)
### Miscellaneous
- Init versioning ([`f1404a1`](https://github.com/szaghi/FLAP/commit/f1404a13f6102767f59756d329da25a07f1b1e56))


