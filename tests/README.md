# Unit Tests

## Guide to Run Unit Tests for scheme
```
TeXmacs -x "(run-all-tests)" -q
```
or launch a Scheme session and then run `(run-all-tests)`.

## Guide to Run Unit Tests for cpp

First, compile the whole project.
```
cd texmacs/
mkdir build/ && cd build/
cmake ..
make -j8
```

Then, run your unit tests:
```
ctest // run all
ctest -R analyze // run unit tests with name containing `analyze`
```

### Advanced Topic
You may also run the unit tests via the binaries under `${cmake_build_dir}/tests/`
``` bash
tests/converter_test
```

However, this specify unit test will fail. For `utf8_to_cork`, we need to set
the `TEXMACS_PATH` to find the dictionaries. You may specify it manually:
``` bash
TEXMACS_PATH=/path/to/somewhere tests/converter_test
```

Or just using ctest(we've set the necessary environment variables):
``` bash
ctest -R converter_test
```

## Unit tests with the autotools build

The CMake harness above needs a CMake build. With the usual
`./configure && make` build, `tests/Makefile` compiles the same test sources
against the objects in `src/Objects` and QtTest:

```
make -C tests                      # build and run all tests
make -C tests run-tt_tools_test    # one test, with QtTest output
make -C tests TM_TEST_FONT_DIR=/path/to/fonts
```

`TM_TEST_FONT_DIR` points to a directory (searched recursively) with extra
fonts that some tests need, for instance Latin Modern Math and STIX Two Math
for the OpenType tests; those tests are skipped when the fonts are missing.
The tests run with `TEXMACS_PATH` set to the source tree and a scratch
`TEXMACS_HOME_PATH` under `tests/build`, so they never touch `~/.TeXmacs`.

Two test sources are left out of this harness: `xml_test`, which includes a
source file that is already part of the main build, and `mac_images_test`,
whose functions `mac_images.h` does not declare in a Qt 6 build.

Because dependency tracking may be disabled in the main build, run
`make -C tests check-stale` after changing a header and remove the listed
objects before rebuilding.

## Visual regression for math typesetting

`tests/opentype/render-samples.sh` renders every document in
`tests/opentype/samples/` to PDF and PNG (one PNG per page, via mutool) in
`tests/build/vis`, with the git revision in the file name:

```
TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/render-samples.sh
tests/opentype/render-samples.sh -c reference-dir   # pixel diff with ImageMagick
```

`tests/opentype/check.sh` runs the unit tests and both sample renders, tuned
and untuned, and is the script to run before a commit. When `tests/build/ref`
exists it diffs the renders against it and prints the number of differing
pixels; refresh it on purpose, by copying the accepted renders of
`tests/build/vis` over it under their plain names (`math-overview-1.png`,
`math-showcase-3.png`, ...).

`tests/opentype/compare-lualatex.sh` typesets the formula pairs of
`tests/opentype/compare/` twice with the same OpenType math font, once
through `unicode-math` under LuaLaTeX and once through TeXmacs, and stacks
the two renders in one PNG so they can be compared line by line:

```
tests/opentype/compare-lualatex.sh            # all pairs
tests/opentype/compare-lualatex.sh radicals-bars
```

The sample `math-overview.tm` typesets the same formulas with TeX fonts,
the shipped TeX Gyre and STIX fonts, and several OpenType math fonts, so the
effect of a change on each code path can be compared side by side.
