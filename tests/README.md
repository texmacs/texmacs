# Unit Tests

## Guide to Run Unit Tests for scheme
```
TeXmacs -x "(run-all-tests)" -q
```
or launch a Scheme session and then run `(run-all-tests)`.

## Guide to Run Unit Tests for cpp

First, compile the whole project with the tests enabled. That harness is
built against Qt 5 (`find_package(Qt5Test)`), so it needs a Qt 5
configuration; with Qt 6 use the autotools harness described below.
```
cd texmacs/
mkdir build/ && cd build/
cmake -DBUILD_TESTS=ON ..
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
against the objects in `src/Objects`:

```
make -C tests                      # build and run all tests
make -C tests run-tt_tools_test    # one test, with its own output
make -C tests TM_TEST_FONT_DIR=/path/to/fonts
```

The tests inherited from the CMake harness are written against QtTest, which
is what `tests/CMakeLists.txt` expects of them. New tests are written in
ordinary TeXmacs C++ over `tests/tm_test.hpp`, which brings `CHECK`,
`CHECK_MSG`, `CHECK_EQ` and `SKIP`, and a `main` which lists its tests with
`RUN` and returns `test_report ()`: a test then needs no framework, and the
Qt idioms stay in the Qt port where they belong. `tests/Makefile` builds both
kinds, and runs `moc` only on the sources which declare a `Q_OBJECT`.

`TM_TEST_FONT_DIR` points to a directory, searched recursively, with the
fonts the OpenType tests need. Latin Modern Math and STIX Two Math are
shipped in the tree now, but the tests still look for them through this
variable and skip themselves when it is not set; other fonts, Asana Math for
instance, are genuinely external.
The tests run with `TEXMACS_PATH` set to the source tree and a scratch
`TEXMACS_HOME_PATH` under `tests/build`, so they never touch `~/.TeXmacs`.

Two test sources are left out of this harness: `xml_test`, which includes a
source file that is already part of the main build, and `mac_images_test`,
whose functions `mac_images.h` does not declare in a Qt 6 build.

The renders and the pixel diffs depend on the PDF writer of the build. With
the native (Hummus) PDF renderer the exported fonts are real subsets, and
without it every glyph becomes a Type 3 bitmap, which changes the
anti-aliasing of every page. Configure with

```
CPPFLAGS='-I/opt/homebrew/opt/libpng/include/libpng16 -I/opt/homebrew/include'
```

on macOS to get the native renderer ("hummus support for native pdf
exports... enabled"), and rerun `make SAFE_TEXMACS_REV` afterwards, since a
reconfigure overwrites `TeXmacs/SVNREV` with the output of `svnversion`.

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

`tests/opentype/missing-symbols.py` answers a different question: which
mathematical symbols TeXmacs has no name for. It reads the symbol list of
`unicode-math` and the tables of `TeXmacs/langs/encoding` and writes
`doc/math-symbol-coverage.md`:

```
python3 tests/opentype/missing-symbols.py \
  -t /path/to/unicode-math-table.tex -o doc/math-symbol-coverage.md
```

With `--emit` it prints draft Scheme for a chosen family instead: the lines
to put in place of the comments that hold their place in the encoding table,
and a `std-symbols.scm` group built from the unicode-math class, which is
what gives a symbol its spacing.

```
python3 tests/opentype/missing-symbols.py -t /path/to/unicode-math-table.tex \
  --emit --block "Mathematical operators" --min-fonts 10
```

With `--check` it verifies the tables themselves, that no name is given two
code points and no code point two two-way names, and exits non-zero on a
failure. `check.sh` runs it when `TM_UNICODE_MATH_TABLE` points at the list.

With `--tables TeXmacs/langs/encoding` it writes two proposal tables there,
in the shape of the tables beside them: `tmuniversaltounicode-extra.scm`
with the entries that are ready, and
`tmuniversaltounicode-extra-candidates.scm` with the rest, every line
commented and carrying the reason. `confirmed-symbols.txt` next to the
script lists the names whose shape was compared by eye with the glyph of
the code point, which is what moves them from the second file to the
first. Nothing loads them until a
`hashtree_from_dictionary` line names the first one in
`src/Data/String/converter.cpp`.


The sample `math-overview.tm` typesets the same formulas with TeX fonts,
the shipped TeX Gyre and STIX fonts, and several OpenType math fonts, so the
effect of a change on each code path can be compared side by side.
`math-showcase.tm` tours every MATH feature font by font, and
`math-variants.tm` shows math roman, math sans serif, math typewriter and
bold mathematics for the profiled fonts. `math-symbols-extra.tm`, generated
by `missing-symbols.py --sample`, shows the 200 symbols of
`tmuniversaltounicode-extra.scm` in tables: a name the conversion tables
fail to serve appears there as a box instead of a glyph.
