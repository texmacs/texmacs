# 5. Build, the vendored s7, and the branch

## 5.1 Choosing the interpreter

The interpreter is a build option, and **s7 is the default**. An s7 build
needs no Guile at all: nothing to install and nothing linked, and the glue
is regenerated with s7 too.

| Build system | s7 (default) | Guile |
|---|---|---|
| autotools | `./configure` or `./configure --with-scheme=s7` | `./configure --with-scheme=guile` |
| CMake | `-DSCHEME_IMPL=s7` | `-DSCHEME_IMPL=guile` (any version), or `guile-1.8`, `guile-2.0`, `guile-2.2`, `guile-3.0` |

The option drives everything else:

- **Macros:** it defines `USE_S7` or `USE_GUILE` in `config.h` (from
  `misc/m4/scheme.m4` and `config.h.cmake`).
- **Sources:** it picks the backend directory, `SCHEME_DIR=S7` or `Guile`.
  `src/makefile.in` compiles `src/Scheme/{Scheme,$(SCHEME_DIR)}`, and CMake
  globs the same directory.
- **C++:** `object.hpp` includes `s7_tm.hpp` or `guile_tm.hpp`, and the
  backend names its init file, `init-s7.scm` or `init-guile.scm` (§2.1).
- **Guile detection:** `LC_GUILE` (detection, flags, `-lguile`) runs only
  for Guile.
- **Platform code:** the Guile hooks in the Unix, Windows64 and Android
  files are compiled only with `USE_GUILE`.
- **Packaging:** the rules in the top-level `Makefile.in` copy Guile's
  `ice-9` directory only when there is one.

**Both interpreters build, boot and pass the tests.** This was checked with
autotools, on macOS, with Guile 1.8.7. Guile runs all the regression suites
except the two that test s7 specifically. Linux and Windows builds have not
been tried yet (see [06](06-open-issues.md)).

**CMake.** Upstream's CMake build had gaps that stayed hidden while an
autotools `config.h` was left in the source tree. Three were fixed:
- the generated `config.h` now comes first in the include path;
- `config.h.cmake` defines `SIZEOF_{SHORT,INT,LONG,LONG_LONG}` and
  `ALTERNATIVE_VERSION`;
- the Guile checks pass `Guile_CFLAGS` as a space-separated string.

Out of tree, the CMake build still stops at `System/Files/web_files.cpp`,
which includes the Qt header `qt_utilities.hpp` from outside the core
library's include path. That upstream problem doesn't depend on the Scheme
choice.

**Xcode.** `packages/macos/TeXmacs.xcodeproj` compiles `s7_tm.cpp` and
`s7.c` in three targets.

<a id="glue-regeneration"></a>
### Glue regeneration

The generated `glue_*.cpp` files work with either interpreter, and so do
the generators (`build-glue.scm`, `make-apidoc-*.scm`).

- **On s7,** `make -C src GLUE` first builds `Objects/s7-run`: a small
  command-line s7, built from `src/Scheme/Glue/s7-run.c` and the vendored
  `s7.c`, that accepts Guile's `-l FILE -c EXPR` options. Its output is
  byte-identical to the committed files.
- **Failures don't truncate the glue.** `build-glue` and `build-auto-doc`
  write to a temporary file and replace their output only when the
  generator succeeds.

### After a rebase

- **Run `make -C src clean` before building.** Upstream changes function
  signatures in headers, and stale objects then fail at link time.
- **Untracked files are expected.** `configure` also generates a few
  (`packages/msix/*.xml`, `packages/android/res/values/`).
- **`configure` was regenerated with Autoconf 2.73**; the previous one was
  made with 2.72. Most of its diff is version noise.

<a id="s7-version-and-local-patch"></a>
## 5.2 The vendored s7

`src/Scheme/S7/s7.c` and `s7.h` are **s7 11.9 (21-Sep-2026) as released**
(`https://ccrma.stanford.edu/software/s7/s7.tar.gz`), byte for byte.
`mus-config.h` is an empty placeholder that `s7.c` includes.

- **Compiled as C.** s7 11.9 no longer compiles as C++: in C++ mode it
  disables complex numbers, and its stub `clog` then becomes ambiguous under
  clang++. `src/makefile.in` compiles `src/Scheme/S7/*.c` with the C
  compiler (`cc_incl`), as CMake does.
- **Default options.** No `-DWITH_*` flags are set, so s7 builds with its
  defaults: `WITH_GMP 0`, `WITH_PURE_S7 0`, `WITH_SYSTEM_EXTRAS 1`,
  `WITH_HISTORY 0`, `WITH_WARNINGS 0`, `WITH_MAIN 0`.
- **Runtime settings,** made in `start_scheme` (§1.3):
  `(*s7* 'symbol-quote?)` is `#t`, and the initial heap is 1 M cells.

**s7 11 behaviors that TeXmacs adapts to** (see §2.1–2.3 and
[03](03-compat-layer.md)):
- the reading of `'x`;
- `varlet` refusing already-bound symbols;
- read-time expansions, which TeXmacs no longer uses;
- a `load` during an expansion ending the outer load;
- internal definitions in macro bodies;
- how lookups use let ids.

**No local patch.** Earlier versions of the port patched s7's symbol
lookup:
- first by moving found slots to the front of their let, which was unsound,
  because it reordered lets that s7 iterates over or refills by position;
- then with an id check.

Both patches were needed only because TeXmacs copied every export into one
huge user environment. Since public definitions are published in the rootlet
(§2.2–2.3), stock s7 is as fast as the patched one. The tests of the
`lookup` group in `boot-s7-test.scm` still check the two properties the
first patch broke.

### Upgrading s7

1. Copy the new `s7.c` and `s7.h` into `src/Scheme/S7`.
2. Rebuild from clean.
3. Run `run-all-tests` and the portable suites on both interpreters (§4.4).
4. Check the timings of [07](07-performance.md), at least boot and the LaTeX
   export loop. The module system relies on how s7 caches lookups (§2.3), so
   a change there would show up as a slowdown, not as a failure.

## 5.3 The branch

`wip_s7` is the upstream snapshot `svn_sync_20260921` (`fa8da19dd0`,
2026-09-17) plus a linear series:

1. **`b23f01c12c` "S7 Scheme support (squashed from wip_s7)":** the whole
   original port as one commit, with its conflicts resolved against the
   newer upstream.
2. **Fixes and new work,** each in its own commit:
   - bug fixes, the update to s7 11.9 and the tests;
   - the build option and the kernel shared with Guile;
   - the module-system and lookup work;
   - the init files, the `latex-needs?` cache and the HTML export fixes.
3. **Commits whose subject starts with `docs/s7:`,** which only touch
   these notes.

The original history, with all commits and authors, is kept on the branch
**`wip_s7_pre_rebase_20260924`** (`dd11d3310a`). In short:
- the port was written in 2020–2022 by Massimiliano Gubinelli;
- it was imported into the TeXmacs repository by Darcy Shen (沈达) in
  November 2021, with CMake support and fixes;
- s7 was updated in January 2022;
- the branch was rebased onto upstream in July 2025.

**To rebase onto a later snapshot**, run
`git rebase --onto <new-snapshot> <old-snapshot>`. Then:

1. **Merge upstream's changes to the init files.** Upstream edits
   `init-texmacs.scm`, which is split here into four files:
   - changes to its start belong in `init-guile.scm`, and possibly in
     `init-s7.scm`;
   - changes to the kernel imports belong in `init-kernel.scm`;
   - the rest stays in `init-texmacs.scm`.
2. **Check new upstream Scheme code for Guile-only builtins,** such as
   SRFI-13/14 functions, `(ice-9 …)` modules or `procedure-property`. Add
   what's missing to `compat-s7.scm`.
3. **Rebuild from clean and run the tests on both interpreters.**
