# 7. s7 versus Guile on typical TeXmacs workloads

Measured on 2026-09-24 and 2026-09-25 on macOS (Apple silicon):

- **s7 build:** s7 11.9 with the id-check lookup patch;
- **Guile build:** Guile 1.8.7.

Both builds come from the same tree (`4ae073ff9a` and later), share the Scheme
kernel, and run offscreen (`QT_QPA_PLATFORM=offscreen`) on the same `TeXmacs/`
directory. Every run was capped at 1.5 GB RSS and killed on timeout, and the
runs were made one after the other on an otherwise idle machine.

## Results

The tables give s7 before and after the module-lookup fix described below.
The before/after columns and the Guile column were measured in the same
session on 2026-09-25, alternating builds.

| Workload | s7 before | s7 after | Guile |
|---|---:|---:|---:|
| Boot (launch → quit, 5 runs) | 0.63 s | 0.64 s | 1.54–1.64 s |
| 15 portable regression suites | 187–189 ms | 190–200 ms | 269–276 ms |
| 8 warm LaTeX exports of the change log | 9.2–9.3 s | 4.9–5.1 s | 7.7–7.8 s |
| `fib 25` (pure interpreter) | < 1 ms | < 1 ms | 60–84 ms |

The conversion benchmark uses four documents: the change log, `env-page`,
`bigtable-test` and `superscript-test-bis`. The benchmark script:

- runs each task once cold, then three more times, and reports the median of
  the three warm runs in ms;
- exports through an auxiliary buffer, as `check-latex-export` does.

Two runs per build, with the ranges shown when they differ:

| Task | s7 cold | Guile cold | s7 warm | Guile warm | s7 warm before the fix |
|---|---:|---:|---:|---:|---:|
| load 4 docs | 178–188 | 175–184 | 14 | 13 | 13 |
| tree → stree → tree | 8–9 | 9–10 | 8 | 9 | 8 |
| export LaTeX | 3037–3211 | 4590–4825 | 2433–2532 | 2818–2861 | 3525 |
| export HTML | 4732–4811 | 5951–6131 | 3423–3496 | 3904–3932 | 3445 |
| import LaTeX | 558–596 | 1710–1922 | 313–339 | 312–317 | 326 |
| import HTML | 817–878 | 1043–1053 | 784–852 | 969–1011 | 911 |
| menu expansion ×10 | 18–19 | 74–78 | 18 | 31 | 34 |

**Peak RSS:** about 970 MB on s7 after the fix, 840 MB before the fix, and
785 MB on Guile. See [Memory](#memory) for the difference.

## Reading the numbers

- **Anything that loads code is much faster on s7:** boot, and the cold runs
  that trigger lazy module loading (LaTeX import 0.6 s vs 1.7–1.9 s, menus).
  s7 reads and evaluates source faster than Guile 1.8, which has no compiler
  and a slow reader.
- **Warm conversions are at parity or better on s7,** because most of the
  time is spent in C++ (the typesetter, the parsers, the tree code).
- **The warm LaTeX export was 20–25% slower on s7 before the fix, and is now
  about 35% faster.** The cause is described next.

## Why the warm LaTeX export was slow

### Measurements

A sampling profile of 8 warm exports showed three main costs:

- `c_function_is_ok`: about 1050 samples. Its cost is the symbol `lookup`
  inlined into it.
- `eval`: about 860 samples.
- the garbage collector: about 600 samples.

A counting build of `lookup` (see §5.2) gives, for the same loop:

- **Lookups:** 301 M, of which only 13 M fall through to the global
  environment.
- **Walks:** 164 M environment frames and **5.8 G** slot comparisons.
- **Where the comparisons happen:** 5.7 G of them are in frames that do not
  hold the symbol, on the way to the frame that does. So the scan that finds
  the symbol is short, but the walk that leads to it is not.
- **Which symbol:** `==` alone accounts for 4.76 G, over 19.9 M lookups.
  `ahash-ref`, `list?` and `func?` account for another 0.67 G.
- **Which frame:** 89% of these comparisons are in a single frame, the
  environment of the module `(convert latex tmtex)`, which holds about 360
  bindings. It is walked 12 M times.

### The mechanism

Every module is a `sublet` of the user module `*texmacs-user-module*`, into
which the kernel is imported. A lookup of `==` from code in `tmtex` walks
the `tmtex` environment, then reaches the user module, where the id check
(§5.2) finds `==` in O(1).

s7 does not scan the `tmtex` environment if it is newer than the symbol's
cached binding (`let_id > symbol_id`). Normally that holds: the module is
created after the kernel is imported. It stopped holding because of
`tm-define-macro`:

- On s7, `tm-define-macro` expanded to `(with-module *texmacs-user-module*
  (define-public-macro …))`.
- `with-module` is a `with-let`, and `with-let` gives the let it enters a
  fresh, highest id. It also points the cached binding of every symbol of
  that let at it.
- So each time a module containing a `tm-define-macro` was loaded, the user
  module became newer than every module loaded before it. This happened 211
  times during the benchmark, the last three times (`with-user`,
  `with-encoding`, `with-limit`) during the first export, after `tmtex` had
  been loaded.
- From then on, every lookup of a kernel symbol from `tmtex` code scanned the
  360 bindings of `tmtex` first.

This is not specific to the benchmark: in a normal session, modules and the
macros they define are loaded lazily, and more and more modules end up in
this state.

### The fix

Three changes to the s7 kernel:

1. **`tm-define-macro`** defines the macro with `(eval '(define-public-macro
   …) *texmacs-user-module*)`. `eval` sets the current environment without
   renumbering it.
   - When the symbol is already cached in a newer let, s7's `define` gives
     it a fresh dummy id, which forces later lookups to scan instead of
     skipping a binding. So this stays sound.
   - A variant that built the macro with `(macro …)` and `varlet` broke the
     second of two consecutive LaTeX exports. The cause was not
     investigated.
2. **`renumber-user-module!`** (in `boot-s7.scm`) is called exactly once, in
   `init-texmacs-s7.scm`, right after the kernel is imported. The kernel
   symbols are then cached in the user module, and every module loaded
   afterwards is newer, so lookups skip it.
   - Without this step the kernel symbols stay cached in their defining
     modules. Then every lookup scans the ~970 bindings of the user module:
     18 G slot comparisons and a slower export (42.5 s instrumented, against
     26.5 s before the fix).
   - Doing it at the end of boot instead gives the same results on these
     workloads.
3. **`import-bindings!`** no longer copies into a module a binding that the
   module already sees, with the same value, through the user module.
   - Many modules import kernel modules themselves. The copy did not change
     what lookups return, but it moved the symbol's cache into the (newer)
     importing module, so lookups from everywhere else had to scan the user
     module again.
   - The repeated renumbering had been hiding this, because it kept pulling
     the caches back.
   - With changes 1 and 2 alone, the regression suites slowed from 188 to
     about 235 ms (45 M slot comparisons against 7.7 M). With change 3 they
     are back at about 195 ms (10.5 M comparisons).

**Result:** slot comparisons in the 8-export loop drop from 5.8 G to 0.69 G,
and the loop takes 4.9 s instead of 9.2 s.

The fix is covered by new tests in `boot-s7-test.scm` (group `modules`).

<a id="memory"></a>
### Memory

After the fix, peak RSS is higher: about 420 MB instead of 286 MB for the
8-export loop, and 176 MB instead of 146 MB for the suites. Nothing is
retained:

- the live data after a GC is the same, about 425 k cells;
- 25 exports peak at 437 MB, against 415 MB for one.

The difference is s7's heap-growth policy. After a GC, if less than 80% of
the heap is free, the heap doubles. It quadruples instead if less than 67% is
free (`gc-resize-heap-by-4-fraction`).

- **Before the fix,** the heap had grown to 2 M cells during boot.
- **Now,** it is still 1 M cells after boot, and quadruples to 4 M during
  the first export.

Setting `(*s7* 'gc-resize-heap-by-4-fraction)` to 0.3 keeps the heap at 2 M
cells and the peak at about 290 MB. The loop then takes 5.6 s instead of
4.9 s, because the smaller heap needs more collections. This trade-off is
not applied.

## Portability gaps found by the benchmark

The HTML export hung on s7. The export code used `hash-map->list` and
`string-prefix?`, which `compat-s7.scm` did not provide, and it recognised
ornament keywords by their printed form (`#:ornament-…`), which differs
between the two interpreters. Both are fixed (`414652945b`).

The HTML files each build exports are now the same, except for:

- the order of attributes, which follows hash-table iteration order;
- float printing: s7 prints the shortest round-trip form, up to 17
  significant digits (`-0.012412121212121213em`), while Guile prints 15
  (`-0.0124121212121212em`).

## Reproducing

The scripts are small TeXmacs Scheme files that are loaded with
`texmacs.bin -x "(load \"…\")"`:

- **Suites:** the 15 `regtest-*` suites timed with `texmacs-time`.
- **Conversions:** `bench` wraps a thunk. It prints the cold time and the
  median of three warm runs.

The Guile binary is built with `./configure --with-scheme=guile`, from a copy
of the tree, and pointed at the same `TEXMACS_PATH`. Timings are only
meaningful on an idle machine: at load average 260, the same HTML export
took 13 s cold instead of 4.4 s.
