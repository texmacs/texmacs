# 5. Build integration, the vendored s7, and history

## 5.1 How s7 is built and selected

There is **no preprocessor switch or configure option**. s7 is selected in
three places:

1. `src/Scheme/Scheme/object.hpp:17-19` includes `../S7/s7_tm.hpp`.
2. The build system compiles `src/Scheme/S7` instead of `src/Scheme/Guile`.
3. `src/Texmacs/Server/tm_server.cpp:138` loads `init-texmacs-s7.scm`.

### Autotools (the main build here)

- **`src/makefile.in:123-124`:**
  `scheme_src := $(call findsrc_in,Scheme,Scheme S7)`, with the same pattern
  for `scheme_c_src`.
- **`s7.c` is compiled with the C++ compiler** (`src/makefile.in:346`).
- **`configure.in` still runs only `LC_GUILE`.** It still requires Guile
  (`configure` stops with "cannot work without Guile"). It still defines
  `GUILE_A`…`GUILE_D` and `GUILE_VERSION`, and still **links** `-lguile`
  through `LIBS`.
- **Some platform files still include `Guile/guile_tm.hpp`** and call Guile
  hooks: `Plugins/Unix/unix_system.cpp` and `unix_entrypoint.cpp`, and the
  Windows64 and Android equivalents.
- **`init_texmacs.cpp:init_guile`** still sets `GUILE_LOAD_PATH` and checks
  that `init-texmacs.scm` exists. `boot-s7.scm` reuses `$GUILE_LOAD_PATH`
  to resolve module files.

So a Guile install (1.8 on `PATH`) is still needed to configure and link,
even though Guile is never initialized. Guile is also needed to regenerate the
glue.

**After a rebase onto a new upstream snapshot**, run `make -C src clean`
before building. Upstream changes function signatures in headers (for example
`concretize(url)`), and stale objects then fail at link time with undefined
symbols. `configure` also generates a few untracked files
(`packages/msix/*.xml`, `packages/android/res/values/`), which are expected.

### CMake

- **`SCHEME_IMPL=s7` fails.** `CMakeLists.txt:293-310` offers it, but
  choosing it gives `FATAL_ERROR "…not implemented yet."`.
- **The working path is `SCHEME_IMPL=default`**, which still runs
  `pkg_search_module(Guile REQUIRED …)`. The Guile include directories and
  libraries are commented out, and `src/Scheme/S7/*.{c,cpp}` are added
  (`:423-472`, `:559`). From commit `be5e8ad233`.

### Xcode

`packages/macos/TeXmacs.xcodeproj` compiles `s7_tm.cpp` and `s7.c` in three
targets.

### s7 compile options

- **No `-DWITH_*` flags are set anywhere**, so s7 builds with its defaults:
  `WITH_GMP 0`, `WITH_PURE_S7 0`, `WITH_SYSTEM_EXTRAS 1`, `WITH_HISTORY 0`,
  `WITH_WARNINGS 0`, `WITH_MAIN 0`.
- **`mus-config.h` is an empty placeholder.** It exists because `s7.c`
  includes it.

<a id="s7-version-and-local-patch"></a>
## 5.2 s7 version and the local patch

### Version

`s7.h`: `S7_VERSION "11.9"`, `S7_DATE "21-Sep-2026"`. This is the current
release from `https://ccrma.stanford.edu/software/s7/s7.tar.gz`, vendored on
2026-09-24. Before that, the vendored version was 10.0 (11-Jan-2022).

**`s7.c` is compiled as C.** s7 11.9 no longer compiles as C++: in C++ mode
it disables complex numbers, and its stub `clog` then becomes ambiguous
under clang++. `src/makefile.in` therefore compiles `src/Scheme/S7/*.c` with
the C compiler (`cc_incl`). CMake already compiled it as C.

**s7 11 behavior changes that TeXmacs had to adapt to**, all described in
§2.1–2.2 and [03](03-compat-layer.md):

- `(*s7* 'symbol-quote?)`;
- `varlet` on already-bound symbols;
- read-time expansions (no longer used);
- nested loads during expansions;
- internal definitions in macro bodies.

### The patch in use: `s7-lookup_from.patch`

In `inline_lookup_from` (about `s7.c:11589` in 11.9), while searching a
non-global `let`, the lookup counts the slots it walks. If the symbol is more
than 100 slots deep, its slot moves to the front of that `let`:

```c
if ((steps > 100) && (let != sc->rootlet))
  {
    slot_set_next(prev, next_slot(slot));
    slot_set_next(slot, let_slots(let));
    let_set_slots(let, slot);
  }
```

The 11.9 port has two changes from the 10.0 version:

- It uses s7 11's setter macros, which are checked in s7's debug builds.
- It never touches the rootlet, whose slot list s7 11 asserts is never set.

Moving slots is only safe in big lets. Those are module environments, never
the lets of function arguments, which s7 may access by position.

- **Why it is needed.** `*texmacs-user-module*` holds thousands of imported
  bindings (see §2.2).
- **Effect.** According to `README.md`, it brought manual typesetting down to
  about 15 s on s7 10, the same as Guile 1.8, and halved startup time. It is
  still worth it on 11.9. Startup up to the end of the forced delayed loads
  (the `time:` line printed at boot) takes about 820–850 ms with the patch
  and about 1290 ms without it. On s7 10 it took 517 ms.
- **Unused variant.** `s7-lookup_from-version-2.patch` (a reverse diff) moves
  the slot halfway to the front, with a threshold of 20. It is not applied,
  and it is written against s7 10.0.

### `s7.c.orig` / `s7.h.orig`

These are the pristine upstream 11.9 files. `diff s7.c.orig s7.c` shows
exactly the local patch, and `s7-lookup_from.patch` is that diff.
`patch -p1 < src/Scheme/S7/s7-lookup_from.patch` from the repository root,
applied to pristine 11.9, reproduces `s7.c`. `s7.h` is unmodified.

**To upgrade s7 again:**

1. Copy the new upstream `s7.c` and `s7.h` over both the working files and
   the `.orig` files.
2. Re-apply or port the patch, and regenerate it with
   `git diff --no-index s7.c.orig s7.c`.
3. Rebuild.
4. Run the probe tests from [06](06-open-issues.md).

## 5.3 History

### Current branch layout (since 2026-09-24)

`wip_s7` is now the upstream snapshot `svn_sync_20260921` (`fa8da19dd0`,
2026-09-17) plus a short linear series:

| Commit | What |
|---|---|
| `S7 Scheme support (squashed from wip_s7)` | The whole port as one commit: the net difference between the old `wip_s7` and `svn_sync` (`dd84fcf559`). It also includes conflict resolutions against the 889 newer upstream commits and a resync of `init-texmacs-s7.scm` with the current `init-texmacs.scm`. |
| `S7: fix ahash-size, property on procedures, catch adapter` | Bug fixes (see [06](06-open-issues.md)) |
| `Add docs/s7: notes on the s7 port` | These notes |
| `S7: support the uint glue type` | Replaces a direct Guile call in `glue.cpp`; needed by new upstream glue |
| `S7: char-sets and *random-state* for the new server code` | Compat additions for new upstream Scheme code |

The old history, with all the original commits and their authors, is kept
on the branch **`wip_s7_pre_rebase_20260924`** (`dd11d3310a`). That history
did not share commits with `svn_sync`: the July 2025 rebase had replayed
upstream under different hashes. The squash made it possible to rebase with
an exact merge base.

**To rebase onto a later snapshot**, run
`git rebase --onto <new-snapshot> <old-snapshot>`. After that:

1. Re-sync `init-texmacs-s7.scm` with `init-texmacs.scm` (see
   [06](06-open-issues.md)).
2. Rebuild from clean.
3. Run the probe tests.

### Old history

- **Commit count.** Before the squash, `git log master..HEAD` showed 618
  commits, but `git cherry master HEAD` found only about 34 that were not
  already upstream.

| Date | Commit | Author | What |
|---|---|---|---|
| 2020–Jan 2021 | (in mgubi/texmacs) | M. Gubinelli | Original port. `README.md` is dated January 2021 and includes r7rs benchmark tables (s7 vs chibi, chez, guile 1.8 / 3.0). |
| 2021-11-06 | `29481b3441` | 沈达 (Darcy Shen) | "S7 support imported from mgubi/texmacs": `s7.c`/`s7.h`, `s7_tm.*`, the patches, `init-texmacs-s7.scm`, `boot-s7.scm`, `compat-s7.scm`, and about 20 edited progs files |
| 2021-11-06 | `be5e8ad233` | 沈达 | CMake support |
| Nov–Dec 2021 | `f79bf1253d`, `03eeaaabc1`, `441a842861`, `ad5af1cf85`, `374cbbf4fc`, `f30e514a75` | 沈达 | Fixes: `string-split`, `ahash-size` typo, `string-contains`, `procedure-name` test, plugin list |
| 2022-01-03 | `b8315794fe`, `a048bba0e1` | 沈达 | `compute-interactive-args`; `procedure-symbol-name` for anonymous procedures |
| 2022-01-12 | `9431bebb70` | M. Gubinelli | Updated s7 to the current upstream version |
| 2022-01-14/15 | `6497fbc93c`, `edc14367b1`, `59763b2975`, `9e2b669de3` | M. Gubinelli | Multiple values in `with-global`, the `.orig` files, `iota`, Guile/s7 compatibility in `abbrevs.scm` |
| 2022-01 / 2023-01 | `ec43ea0a32`, `0c537926fb` | M. Gubinelli | Merges of master |
| 2025-07-24 | `b1f6f26c7c`, `61e84acb77`, `65c32e6974`, `bbe7dfe2b9`, `dd11d3310a` | mgubi | Rebase onto current upstream; re-applied `get-user-name` and the `procedure-name` test; synced `init-texmacs-s7.scm` |

| 2026-09-24 | (see above) | mgubi | Squash, then rebase onto `svn_sync_20260921` |

The vendored interpreter has not changed since January 2022. Since then the
work has been rebasing and keeping `init-texmacs-s7.scm` in step with
`init-texmacs.scm`.
