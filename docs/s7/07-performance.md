# 7. Performance: s7 versus Guile

## 7.1 Setup

- **When:** 2026-09-26, on macOS (Apple silicon).
- **What was compared:**
  - s7: s7 11.9 unmodified;
  - Guile: Guile 1.8.7.
- **Same code:** both builds come from the same tree and run the same
  `TeXmacs/` directory, offscreen (`QT_QPA_PLATFORM=offscreen`).
- **How:**
  - The builds alternated within one session, three rounds for the short
    workloads and two for the manual.
  - Every run was capped at 1.5–2.5 GB RSS and a timeout.
  - The machine was moderately loaded (load average about 4), so absolute
    times are a little high. The comparisons are sound.
- **Scripts:** they are in `docs/s7/bench` (see §7.8).

## 7.2 Summary

| Workload | s7 | Guile | Guile / s7 |
|---|---:|---:|---:|
| Boot (launch → quit) | 0.59–0.71 s | 1.55–1.64 s | 2.6× |
| 15 portable regression suites | 190–208 ms | 277–282 ms (one run at 372) | 1.4× |
| 8 warm LaTeX exports of the change log | 2.81–2.98 s | 7.80–8.80 s | 2.9× |
| Regenerating the manual (124 pages) | 4.55–4.59 s | 5.61–5.87 s | 1.25× |
| Peak memory, LaTeX exports | 292–300 MB | 207–223 MB | 0.7× |
| Peak memory, manual | 488–490 MB | 407–408 MB | 0.8× |

What this shows:
- **s7 is faster on everything measured,** and much faster wherever code is
  loaded or interpreted: boot, the first use of lazily loaded modules, and
  Scheme-heavy conversions.
- **Where C++ dominates, the two are at parity:** typesetting, parsing.
- **s7 uses more memory** (§7.7).

### Regenerating the manual, by phase

`tmdoc-expand-help` builds the book; each update then generates the
auxiliary data (table of contents, index, references) and retypesets.

| Phase | s7 | Guile |
|---|---:|---:|
| tmdoc expansion | 0.73–0.81 s | 1.46–1.52 s |
| first update | 2.23–2.31 s | 2.63–2.76 s |
| second update | 0.75–0.77 s | 0.75–0.82 s |
| third update | 0.76–0.78 s | 0.77–0.78 s |

In this workload typesetting takes about 86% of the time, and Scheme about
16%. That includes the C++ that Scheme calls (loading buffers, generating
the auxiliary data).

### Document conversions

`conversions.scm` works on four documents (the change log, `env-page`,
`bigtable-test`, `superscript-test-bis`). It runs each task once cold, then
three times warm, and reports the median of the warm runs. Times are in ms,
from one run per interpreter.

| Task | s7 cold | Guile cold | s7 warm | Guile warm |
|---|---:|---:|---:|---:|
| load the 4 documents | 187 | 210 | 14 | 13 |
| tree → stree → tree | 9 | 9 | 10 | 9 |
| export LaTeX | 2343 | 3991 | 1255 | 2486 |
| export HTML | 4390 | 4963 | 3299 | 3800 |
| import LaTeX | 603 | 1600 | 320 | 307 |
| import HTML | 681 | 918 | 633 | 950 |
| menu expansion ×10 | 37 | 29 | 16 | 29 |

The cold runs include loading the converter modules, which s7 does much
faster. Peak memory was 966 MB on s7 and 817 MB on Guile.

<a id="boot"></a>
## 7.3 Boot

A sample of an s7 boot (about 0.66 s) shows:

- **Loading the Scheme init files: about 45 ms.**
- **Opening the first window (`open_window`): about 82%.** Within it:
  - about 30% goes to building a `QDockWidget`, because Qt's Fusion style
    loads its standard icons (`QIcon::addFile`);
  - about 25% goes to the Qt font database, including the family aliases
    for the missing "Sans Serif" family that Qt warns about;
  - the rest is menus and toolbars (Scheme), and the first buffer.

So boot is now mostly Qt, and a Guile build pays the same Qt costs.

## 7.4 Where the LaTeX export spends its time

The export runs the Scheme converter `tmtex` on the document tree:
`tree_to_latex_document` takes 90% of the main thread. By self time:
- s7's `eval` takes 45%;
- the GC takes about 20%;
- C++ work takes a few percent.

**One query dominated until it was cached.** `latex-needs?` asks whether a
LaTeX macro needs a package. It is `(logic-ref latex-needs% x)`, a query of
the logic engine.
- `latex-symbol-drd.scm` adds rules such as
  `((latex-needs% 'x "amssymb") (latex-ams-symbol% 'x))`. Their head has a
  free variable, so every query tries them all.
- The converter asked this for every node, often twice: 42 562 calls in 8
  exports, for 71 distinct keys.
- The answers depend only on the logic rules, so `latex-needs?` now caches
  them until rules are added (`logic-rules-version`).
- **Effect:** 8 exports went from 4.7 s to 3.0 s on s7, and from about 9 to
  8.3 s on Guile.
- **Output:** the LaTeX exported from 21 documents is byte-identical with
  and without the cache, on both interpreters.

**What remains:**
- the `tmtex` conversion itself;
- a few similar logic-table lookups (`latex-texmacs-arity`,
  `latex-texmacs-option?`, the catcode definitions), which could be cached
  the same way;
- the GC.

## 7.5 Symbol lookup

With the module system of §2.2–2.3, lookups are a small share of the time.
A counting build of s7's lookup, over 8 LaTeX exports:
- about 0.24 G slot comparisons;
- mostly in small environments: function frames, and the private
  definitions of kernel modules.

Before, the exports were copied into a user module of about a thousand
bindings, and the user module was renumbered as modules loaded. The same
loop then made 5.8 G comparisons and took 9.2 s even with a patched s7.

Profiling showed that the rules of §2.3 are all needed. Without the one
renumbering after the kernel, the loop takes 6.4 s instead of 2.8 s.

<a id="crossing-the-boundary"></a>
## 7.6 Crossing the C++/Scheme boundary

From `marshal.scm`:

| | s7 | Guile |
|---|---:|---:|
| empty loop iteration with a Scheme primitive | 14 ns | 292 ns |
| same with a glue call (`string-alpha? "a"`) | 37 ns | 301 ns |
| string Scheme → C++, 10 B / 1 KB / 100 KB | 0.13 / 0.39 / 35 µs | 0.54 / 0.77 / 35 µs |
| string C++ → Scheme, 10 B / 1 KB / 100 KB | 0.05 / 0.39 / 40 µs | 0.54 / 8.6 / 925 µs |
| `tree->stree`, change log (3 381 nodes, 54 KB of text) | 1.20 ms | 1.32 ms |
| `stree->tree`, same | 1.34 ms | 1.46 ms |

- **On s7, a glue call costs about 20 ns,** and strings about 0.35 ns per
  byte.
- **Guile's own loop overhead dominates its small calls,** and its C++ →
  Scheme strings are much slower on long strings.
- **`tree->stree` and `stree->tree` cost about 400 ns per node** on both,
  because they go through an intermediate C++ tree with quoted strings
  (§1.7).

**How much this matters overall:**
- marshalling is about 3% of the LaTeX export;
- it is under 1% of the manual regeneration.

<a id="memory"></a>
## 7.7 Memory

**s7 peaks higher than Guile:**
- 300 MB against 210 MB for the LaTeX exports;
- 490 MB against 410 MB for the manual;
- 966 MB against 817 MB for the conversions.

**The heap follows s7's growth policy.**
- After a GC, the heap doubles if less than 80% of it is free, or
  quadruples if less than 67% is free (`gc-resize-heap-by-4-fraction`).
- TeXmacs starts s7 with 1 M cells (§1.3). With fewer cells the
  collections are more frequent: the test suites ran 5–10% slower.
- Setting `(*s7* 'gc-resize-heap-by-4-fraction)` lower avoids the jumps to
  a 4× larger heap, trading speed for memory.

**The module system of §2.2 also saves memory.** Copying every export into
the user module made peak memory on the LaTeX exports about 420 MB.

## 7.8 Reproducing

The scripts are in `docs/s7/bench`. Run each with
`texmacs.bin -x '(load "<path>")'`. They quit when done and write their
output files to `$TEXMACS_HOME_PATH/system/tmp/s7-bench`.

| Script | Measures |
|---|---|
| `suites.scm` | the 15 portable regression suites, one by one (`SUITES-TIME`) |
| `latex-loop.scm` | 8 warm LaTeX exports of the change log (`LOOP-DONE`) |
| `manual.scm` | regenerating the manual, by phase (`MANUAL …`) |
| `conversions.scm` | loading, converting and exporting four documents (`BENCH …`) |
| `marshal.scm` | the cost of crossing the C++/Scheme boundary (`MARSHAL …`) |

Measure boot with `time texmacs.bin -x '(quit-TeXmacs)'`.

- **Use a scratch home directory** (`TEXMACS_HOME_PATH`), and
  `QT_QPA_PLATFORM=offscreen` to run without a display.
- **Run the manual once before timing it.** The first run in a fresh home
  directory builds caches: it took 11–15 s instead of about 5.
- **Keep the machine quiet.** At a load average of 260, the same HTML
  export took 13 s cold instead of 4.4 s.

**The Guile build.** Configure it with `--with-scheme=guile` in a copy of
the tree, and point its binary at the same `TEXMACS_PATH`.
