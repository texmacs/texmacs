# TeXmacs on s7: findings on the `wip_s7` branch

These notes describe how this branch replaces Guile with the
[s7](https://ccrma.stanford.edu/software/snd/snd/s7.html) Scheme interpreter.
They are based on reading the tree, `git diff master...HEAD` and the history.
The key semantic claims were checked against the vendored s7 by building it
standalone (see [06-open-issues.md](06-open-issues.md)).

| File | Contents |
|---|---|
| [01-cpp-binding.md](01-cpp-binding.md) | C++ side: `tmscm` abstraction, `s7_tm.cpp`, blackboxes, GC protection, glue generation |
| [02-boot-and-modules.md](02-boot-and-modules.md) | Boot sequence, `init-texmacs-s7.scm`, the environment-based module system in `boot-s7.scm`, `tm-define` |
| [03-compat-layer.md](03-compat-layer.md) | `compat-s7.scm` and the Guile/s7 semantic differences it papers over |
| [04-progs-changes.md](04-progs-changes.md) | Changes to shared Scheme modules under `TeXmacs/progs` |
| [05-build-and-history.md](05-build-and-history.md) | How s7 is built and selected, the local s7 patch, the commit timeline |
| [06-open-issues.md](06-open-issues.md) | Verified bugs, fragile spots, leftover Guile-isms, suggested next steps |

## Summary

- **Selection is compile-time and hard-wired.** `src/Scheme/Scheme/object.hpp:17-19`
  includes `../S7/s7_tm.hpp` in place of the Guile header, and `src/makefile.in:123-124`
  compiles `src/Scheme/{Scheme,S7}`. `tm_server.cpp:138` boots
  `progs/init-texmacs-s7.scm`. There is no `USE_S7` macro, and CMake's
  `SCHEME_IMPL=s7` stops with a fatal error.
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
  `delay`/`force`, `hash`, curried `define`, and more. `init-texmacs-s7.scm` also
  rebinds `define-macro` to s7's `define-expansion`, `symbol?` (keywords are
  excluded), and `load`/`eval`/`catch` (so they use the TeXmacs environment and
  Guile's handler signature).
- **The shared kernel modules are now s7-only.** `ahash-table.scm`,
  `abbrevs.scm`, `tm-define.scm`, `tm-modes.scm`, `srfi.scm`, `debug.scm` and
  others were rewritten in place with no guard. Only the Guile init and boot
  files (`init-texmacs.scm`, `boot.scm`) remain, so the tree no longer boots
  under Guile. Nothing uses `cond-expand` or feature tests.
- **s7 carries one real local patch.** In `lookup_from`, a slot found more
  than 100 slots deep moves to the front of its environment. TeXmacs keeps
  very large environments, and according to `README.md` this cut startup
  time in half and brought manual typesetting down to Guile speed.
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
- **Open upstream s7 bug.** An s7 optimizer bug can mis-apply closures
  called from loops. It is still present in s7 11.9 and is worked around in
  `compat-s7.scm`. See [06-open-issues.md](06-open-issues.md).
