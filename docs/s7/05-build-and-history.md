# 5. Build integration, the vendored s7, and history

## 5.1 How s7 is built and selected

There is **no preprocessor switch or configure option**. s7 is selected in
three places:

1. `src/Scheme/Scheme/object.hpp:17-19` includes `../S7/s7_tm.hpp`.
2. The build system compiles `src/Scheme/S7` instead of `src/Scheme/Guile`.
3. `src/Texmacs/Server/tm_server.cpp:101` loads `init-texmacs-s7.scm`.

### Autotools (the main build here)

- **`src/makefile.in:115-116`:**
  `scheme_src := $(call findsrc_in,Scheme,Scheme S7)`, with the same pattern
  for `scheme_c_src`.
- **`s7.c` is compiled with the C++ compiler** (`src/makefile.in:337-338`).
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

### CMake

- **`SCHEME_IMPL=s7` fails.** `CMakeLists.txt:286-307` offers it, but
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

`s7.h`: `S7_VERSION "10.0"`, `S7_DATE "11-Jan-2022"`.

### The patch in use: `s7-lookup_from.patch`

At `s7.c:10113`, while searching a non-global `let`, the lookup counts the
slots it walks. If the symbol is more than 100 slots deep, its slot moves to
the front of that `let`:

```c
if (steps > 100) {
   next_slot(py) = next_slot(y);
   next_slot(y) = let_slots(e);
   let_slots(e) = y;
}
```

- **Why it is needed.** `*texmacs-user-module*` holds thousands of imported
  bindings (see §2.2).
- **Effect.** According to `README.md`, it brought manual typesetting down to
  about 15 s, the same as Guile 1.8, and halved startup time.
- **Unused variant.** `s7-lookup_from-version-2.patch` (a reverse diff) moves
  the slot halfway to the front, with a threshold of 20. It is not applied.

### `s7.c.orig` / `s7.h.orig`

These were added in `edc14367b1` to track local changes. They are actually a
slightly newer upstream snapshot (14-Jan-2022), so `diff s7.c.orig s7.c`
shows the `lookup_from` patch **plus** upstream drift:

- `FV_BUFSIZE` is different;
- some `is_pair(cdr(error_body))` guards are missing in the local copy;
- the `*s7*` fields `major-version` and `minor-version` are missing in the
  local copy.

To upgrade s7, apply `s7-lookup_from.patch` to the new upstream `s7.c`.

## 5.3 History

- **Commit count.** `git log master..HEAD` shows 618 commits, but
  `git cherry master HEAD` finds only about 34 that are not already upstream.
  The rest came along with the July 2025 rebase.

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

The vendored interpreter has not changed since January 2022. Since then the
work has been rebasing and keeping `init-texmacs-s7.scm` in step with
`init-texmacs.scm`.
