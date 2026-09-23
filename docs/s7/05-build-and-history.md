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

### Why the patch works (measured on 2026-09-24, s7 11.9)

To measure this, `inline_lookup_from` was temporarily instrumented in a
TeXmacs build. The instrumentation counted lookups, walked slots, moves, and
per-let and per-symbol totals, and `TM_NOMOVE` turned the move off in the
same binary. The workload was a boot plus `run-all-tests`.

**How s7 looks up a non-global symbol.**

- **Cached binding.** Each symbol caches one binding: `local_slot`, plus
  `symbol_id`, the id of the let holding it.
- **Let ids.** Every let has an id, and ids normally grow from outer lets to
  inner lets.
- **Lookup order.**
  1. O(1) hit if the current let is the cached one.
  2. Otherwise, skip the lets that are newer than `symbol_id`, and try the
     same O(1) hit on the first let reached.
  3. Otherwise, **linear scan** of the slot lists of the remaining lets.
     Slots are pushed onto the front of a let, so old bindings are at the
     end.

**What happens in TeXmacs.**

- 73% of the 11 M lookups hit the cache directly.
- 21% end up in a linear scan. Without the patch these walk 534 M slots in
  total, and 91% of that (486 M) is spent in a single let: the user module
  `*texmacs-user-module*`, which grows to about 980 bindings.
- The symbols being scanned for are the basic kernel API, imported first and
  therefore at the very end of that list. Positions out of 962: `list?` at
  955, `for` at 865, `with` at 858, `==` at 813, `ahash-ref` at 755.
  - `==` alone costs 124 M slot visits: 168 k lookups at about 734 slots each.
  - Next come `ahash-ref`, `with`, `ahash-set!`, `list?` and `for`.

**Why the cache misses.** Two situations were observed.

1. **The cache points elsewhere** (early in boot, about 1% of the cost). A
   kernel symbol is defined in its module (e.g. `==` in
   `(kernel boot abbrevs)`, let id 1334). It is then imported into the
   *older* user module (let id 35). s7 updates a symbol's cache only when the
   new binding's let is at least as new
   (`if (let_id(let) >= symbol_id(symbol))` in `add_slot_checked_with_id`),
   so the cache keeps pointing into the module, which lookups don't pass
   through.
2. **The cache points into the scanned let** (about 90% of the cost).
   - **Renumbering.** `with-let` and `s7_set_curlet` give the let they enter a
     fresh, highest id, and `update_symbol_ids` points every symbol bound
     there at that let. TeXmacs enters the user module constantly: every
     `eval_scheme`/`call` from C++, `tm-eval`, and `with-module`. So the user
     module gets renumbered and its symbols get cached correctly.
   - **Inverted ids.** After renumbering, the user module is *newer* than the
     module environments and closures created inside it earlier.
   - **The miss.** Starting from such an inner let, the skip loop stops at
     that older inner let, whose id doesn't match. s7 then enters the scan
     loop, which walks all ~950 slots of the user module. It never checks
     that `let_id(let) == symbol_id(symbol)`, which would give the answer in
     O(1) through `local_slot`.

**What move-to-front does.** The user module becomes a self-organizing list.
Only 399 moves happen during the whole run: each hot symbol is moved to the
front about once. From then on it is found within a few dozen slots, because
new bindings only slowly push it back. The average scan for `==` drops from
734 slots to 76. Total slots walked drop from 534 M to 86 M.

| Build (same binary, switches) | Slots walked | Boot, 3 runs (ms) | `run-all-tests`, 3 runs (ms) |
|---|---|---|---|
| stock s7 11.9 | 534 M | 1413 / 1484 / 1450 | 334 / 339 / 312 |
| move-to-front (the patch) | 86 M | 994 / 969 / 961 | 214 / 210 / 240 |
| id check in the scan loop only | 52 M | 1015 / 1027 / 974 | 242 / 241 / 254 |
| both | 48 M | 977 / 985 / 992 | 235 / 237 / 223 |

The times include the instrumentation overhead; the uninstrumented patched
build boots in about 830 ms.

**An alternative fix.** The "id check" rows add one line at the top of the
scan loop, before walking a let's slots:

```c
if (let_id(let) == symbol_id(symbol)) return(local_value(symbol));
```

- **Why it is valid.** This is the same invariant s7 already relies on for
  its O(1) fast path, applied to every let of the chain instead of only the
  first one.
- **What it fixes.** It removes the main cause (case 2) directly and walks
  even fewer slots than move-to-front.
- **What it does not change.** It never reorders slots, so let order and
  functions such as `let->list` behave exactly as in stock s7.
- **Performance.** It performs about the same as move-to-front, and
  combining both gains little.
- **Status.** It looks like a candidate to propose upstream, as a fix of
  s7's own lookup. It has not been adopted yet; the tree still uses the
  move-to-front patch.

<a id="boot-time"></a>
### Boot time (measured on 2026-09-24)

The measurement is the total process time for booting and quitting
(`texmacs.bin -x '(quit-TeXmacs)'`) on the offscreen Qt platform. The table
gives the median of 6 interleaved runs.

| Variant | Median |
|---|---|
| committed tree before 2026-09-24, move-to-front patch | 966 ms |
| same, with a 2M-cell initial heap | 934 ms |
| id check instead of move-to-front | 998 ms |
| **without the debug tail of `init-texmacs-s7.scm`** | **663 ms** |
| without the debug tail, id check | 683 ms |

What the measurements show:

- **The debug tail was a third of the boot.** It consisted of benchmarks and
  forced loading of all keyboard modules; `lazy-keyboard-force` alone took
  about 0.27 s. It is now removed.
- **Symbol lookup no longer matters.** Once either lookup fix is in, the two
  fixes cannot be told apart.
- **A larger initial heap helps a little.** s7 starts with 64000 cells. A
  2M-cell heap gains 10–40 ms but doubles the memory, so it is not used.
- **What remains.** The whole Scheme init file now takes about 65 ms. A
  sample of the boot shows the rest in C++ and Qt, under `open_window`:
  - 233 ms building a `QDockWidget`: the Fusion style loads its standard
    icons through `QIcon::addFile`;
  - 178 ms of font database population for the status bar's font metrics,
    including `populateFamilyAliases` for the missing "Sans Serif" family
    that Qt warns about;
  - the rest in creating and typesetting the first buffer.

  These costs do not depend on s7. They were measured on the offscreen
  platform and may differ with the Cocoa one.

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
