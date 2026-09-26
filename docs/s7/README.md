# TeXmacs on s7: findings on the `wip_s7` branch

These notes describe how this branch replaces Guile with the
[s7](https://ccrma.stanford.edu/software/snd/snd/s7.html) Scheme interpreter.
They are based on reading the tree, `git diff master...HEAD` and the history.
The key semantic claims were checked against the vendored s7 by building it
standalone (see [06-open-issues.md](06-open-issues.md)).

| File | Contents |
|---|---|
| [01-cpp-binding.md](01-cpp-binding.md) | C++ side: `tmscm` abstraction, `s7_tm.cpp`, blackboxes, GC protection, glue generation |
| [02-boot-and-modules.md](02-boot-and-modules.md) | Boot sequence (`init-s7.scm` around the common `init-kernel.scm` and `init-texmacs.scm`), the environment-based module system in `boot-s7.scm`, `tm-define` |
| [03-compat-layer.md](03-compat-layer.md) | `compat-s7.scm` and the Guile/s7 semantic differences it papers over |
| [04-progs-changes.md](04-progs-changes.md) | Changes to shared Scheme modules under `TeXmacs/progs` |
| [05-build-and-history.md](05-build-and-history.md) | How s7 is built and selected, the vendored s7 (unmodified), the former lookup patch, the commit timeline |
| [06-open-issues.md](06-open-issues.md) | Verified bugs, fragile spots, leftover Guile-isms, suggested next steps |
| [07-benchmark.md](07-benchmark.md) | s7 versus Guile 1.8.7 on boot, regression suites and document conversions |

## Summary

- **The interpreter is a build option, and s7 is the default.** Use
  `./configure --with-scheme=s7|guile` or CMake `-DSCHEME_IMPL=s7|guile…`.
  - The option sets `USE_S7` or `USE_GUILE` and the backend directory.
  - An s7 build needs no Guile: nothing to install or link, and glue
    regeneration uses a small `s7-run`.
  - A Guile build (checked with Guile 1.8.7) builds, boots and passes the
    regression tests, except the two suites that test s7 specifically.
  - See [05-build-and-history.md](05-build-and-history.md).
- **The C++ ↔ Scheme boundary barely changed.** TeXmacs already talked to
  Scheme through the `tmscm_*` layer. `s7_tm.hpp/.cpp` (about 600 lines)
  re-implements that layer on the s7 C API. The generated glue
  (`glue_*.cpp`) and `object.cpp` work with either interpreter.
- **The module system is emulated with s7 first-class environments (`let`s).**
  `boot-s7.scm` provides `texmacs-module`, `use-modules`, `inherit-modules`,
  `define-public`, `export` and `with-module` on top of `inlet`, `sublet`,
  `varlet` and a `*modules*` hash table. `tm-define` puts its definitions
  directly into `(rootlet)`.
- **Guile builtins are supplied by `compat-s7.scm`.** It defines `1+`, `noop`,
  `delq`, `acons`, `assoc-ref`, `string-index`, `iota`, `while`, records, R7RS
  `delay`/`force`, `hash`, curried `define`, and more. `init-s7.scm` also
  rebinds `symbol?` (keywords are excluded) and `load`/`eval`/`catch` (so
  they use the TeXmacs environment and Guile's handler signature).
- **One Scheme kernel serves both interpreters.** Shared files test
  `(s7-scheme?)`:
  - at expansion time in the definition macros (`tm-define`,
    `texmacs-modes`, …);
  - or at load time around the few dialect-specific definitions.

  Each interpreter runs the code it ran before. See
  [04-progs-changes.md](04-progs-changes.md).
- **s7 is vendored unmodified (11.9) since 2026-09-26.**
  - **Before:** TeXmacs patched s7's symbol lookup, first with an unsound
    move-to-front patch, then with an id check. `use-modules` copied every
    export into one huge user environment of about a thousand bindings, and
    lookups kept scanning it.
  - **Now:** public definitions are published in the rootlet. The user
    environment holds about 240 bindings, and stock s7 is as fast as the
    patched one. Boot, the tests and the LaTeX export are all a little
    faster than before, and peak memory on large exports is 30% lower.
  - See [02](02-boot-and-modules.md#lookup-caching) and
    [05](05-build-and-history.md#s7-version-and-local-patch).
- **The port is mostly 2021–2022 work.** It was mainly done by M. Gubinelli
  and imported by Darcy Shen (沈达). The branch was rebased onto 2025 upstream
  in July 2025. Of the 618 commits in `master..HEAD`, only about 34 are
  unique (`git cherry`).
- **The branch is now based on `svn_sync_20260921`.** The port was squashed
  into one commit, followed by fix and docs commits. The original history is
  on `wip_s7_pre_rebase_20260924`. See
  [05-build-and-history.md](05-build-and-history.md).
- **Bugs fixed, and tested on a clean rebuild:**
  - `ahash-size`;
  - `property` with procedure arguments;
  - the `catch` adapter;
  - `prog-format`, which failed because the init file had drifted;
  - three Guile dependencies in new upstream code: uint glue, SRFI-14
    char-sets and `*random-state*`.

  All regression and integration suites pass.
- **s7 is faster than Guile 1.8.7 on every workload measured:**
  - boot takes 0.63 s instead of about 1.6 s;
  - the regression suites run 1.4× faster;
  - warm document conversions are at parity or faster.

  Repeated LaTeX export used to be 20–25% slower on s7. `tm-define-macro`
  renumbered the user module, which made every kernel lookup from older
  modules scan their whole environment. Since this was fixed, it is about
  35% faster than on Guile.
- **Most of the LaTeX export time was one repeated logic-engine query.**
  `latex-needs?` was asked about 5 000 times per export for about 70
  distinct keys. It is now cached until logic rules are added, and s7
  exports about 2.7× faster than Guile. See [07-benchmark.md](07-benchmark.md).
- **Open upstream s7 bug.** An s7 optimizer bug can mis-apply closures
  called from loops. It is still present in s7 11.9 and is worked around in
  `compat-s7.scm`. See [06-open-issues.md](06-open-issues.md).
