# 7. s7 versus Guile on typical TeXmacs workloads

Measured on 2026-09-24 on macOS (Apple silicon):

- **s7 build:** s7 11.9 with the id-check lookup patch;
- **Guile build:** Guile 1.8.7.

Both builds come from the same tree (`4ae073ff9a` and later), share the Scheme
kernel, and run offscreen (`QT_QPA_PLATFORM=offscreen`) on the same `TeXmacs/`
directory. Every run was capped at 1.5 GB RSS and killed on timeout, and the
runs were made one after the other on an otherwise idle machine.

## Results

| Workload | s7 | Guile | Ratio (Guile/s7) |
|---|---:|---:|---:|
| Boot (launch → quit, 5 runs) | 0.63 s | 1.54–1.64 s | 2.5× |
| 15 portable regression suites | 193–196 ms | 269–272 ms | 1.4× |
| `fib 25` (pure interpreter) | < 1 ms | 84 ms | — |

The conversion benchmark uses four documents: the change log, `env-page`,
`bigtable-test` and `superscript-test-bis`. The benchmark script:

- runs each task once cold, then three more times, and reports the median of
  the three warm runs in ms;
- exports through an auxiliary buffer, as `check-latex-export` does.

| Task | s7 cold | Guile cold | s7 warm | Guile warm |
|---|---:|---:|---:|---:|
| load 4 docs | 162 | 204 | 13 | 13 |
| tree → stree → tree | 8 | 9 | 8 | 9 |
| export LaTeX | 3854 | 4606 | 3366 | 2742 |
| export HTML | 4449 | 5869 | 3398 | 3847 |
| import LaTeX | 607 | 1668 | 300 | 301 |
| import HTML | 876 | 1032 | 871 | 955 |
| menu expansion ×10 | 18 | 72 | 31 | 29 |

Peak RSS was about 840 MB on s7 and 780 MB on Guile. Most of it is the
typesetter and the documents themselves.

## Reading the numbers

- **Anything that loads code is much faster on s7:** boot, and the cold runs
  that trigger lazy module loading (LaTeX import 0.6 s vs 1.7 s, menus). s7
  reads and evaluates source faster than Guile 1.8, which has no compiler
  and a slow reader.
- **Warm conversions are mostly at parity,** because most of the time is spent
  in C++ (the typesetter, the parsers, the tree code).
- **The one clear loss is the warm LaTeX export, 20–25% slower on s7.** A
  longer run (8 warm exports of the change log) gives 9.5 s on s7 and 9.1 s
  on Guile.

### Where the warm LaTeX export spends its time on s7

A sampling profile of the 8-export loop gives:

- `c_function_is_ok`: about 1050 samples. Its cost is the symbol `lookup`
  inlined into it.
- `eval`: about 860 samples.
- the garbage collector: about 600 samples.

A counting build of `lookup` gives, for the same loop:

- **Lookups:** 301 M, of which only 13 M fall through to the global
  environment. So the cost is **not** builtins shadowed by locals.
- **Walks:** 164 M environment frames and **5.8 G** slot comparisons.
- **Scans that find the symbol are short:** 45.7 M scan ≤ 10 slots,
  1.4 M scan ≤ 100, and 64 k scan ≤ 1000.
- **Top symbols by slots scanned:** `free-variable?` (41 M), `string-starts?`
  (14 M), `tree` (11 M), and the logic engine (`unify-any`, `bind-unify`, …).

Most of the 5.8 G slot comparisons are therefore made while walking frames
that do **not** hold the symbol, on the way to the one that does. The id
check (§5.2) only shortcuts the frame that holds the cached binding.
s7 skips frames newer than the symbol's binding only at the head of the
chain. Further along, the ids need not decrease, because `with-let`
renumbers the frame it enters (§5.2). Extending the skip to the whole chain would need its own soundness
argument before it could be tried.

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
