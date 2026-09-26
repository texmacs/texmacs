# TeXmacs on s7

The `wip_s7` branch lets TeXmacs run its Scheme code on
[s7](https://ccrma.stanford.edu/software/snd/snd/s7.html) instead of
Guile. These notes describe how it works, what differs from Guile, how it
performs, and what is left before it can be merged.

| File | Contents |
|---|---|
| [01-cpp-binding.md](01-cpp-binding.md) | The C++ side: the `tmscm` layer on s7, blackboxes, GC protection, glue, the cost of crossing the boundary |
| [02-boot-and-modules.md](02-boot-and-modules.md) | The boot sequence, the module system built on s7 environments, how lookups stay fast, `tm-define` |
| [03-compat-layer.md](03-compat-layer.md) | `compat-s7.scm` and the Guile/s7 differences it covers |
| [04-progs-changes.md](04-progs-changes.md) | The Scheme code shared by both interpreters, the s7-motivated edits and fixes, the tests |
| [05-build-and-vendored-s7.md](05-build-and-vendored-s7.md) | Choosing the interpreter at build time, glue regeneration, the vendored s7, the branch and how to rebase it, CI |
| [06-open-issues.md](06-open-issues.md) | Open bugs, fragile spots, Guile leftovers, what to do before merging |
| [07-performance.md](07-performance.md) | s7 versus Guile 1.8.7 on boot, tests, conversions, LaTeX export and the manual; where the time goes |
| [bench/](bench) | The benchmark scripts |

## Summary

- **The interpreter is a build option, and s7 is the default.** Choose it
  with `./configure --with-scheme=s7|guile` or CMake `-DSCHEME_IMPL=…`.
  - An s7 build needs no Guile: nothing to install or link, and even the
    glue is regenerated with s7.
  - Both interpreters build, boot and pass the tests. For s7, CI checks
    this on Linux, macOS and Windows, and keeps a runnable build of each
    (see [05](05-build-and-vendored-s7.md#ci)).
- **s7 11.9 is vendored unmodified.**
- **The C++ ↔ Scheme boundary barely changed.** TeXmacs already talked to
  Scheme through its `tmscm_*` layer. `s7_tm.hpp/.cpp` implements that
  layer on the s7 C API, and the generated glue works with either
  interpreter.
- **Modules are s7 environments.** `boot-s7.scm` implements `texmacs-module`,
  `use-modules`, `define-public` and the rest on top of s7's first-class
  environments.
  - Public definitions and `tm-define`s live in the rootlet, where every
    module finds them.
  - A few rules keep s7's lookups fast (see
    [02](02-boot-and-modules.md#lookup-caching)).
- **Guile builtins come from `compat-s7.scm`,** plus a few rebindings in
  `init-s7.scm` (`symbol?`, `load`, `eval`, `catch`).
- **One Scheme code base serves both interpreters.**
  - The C++ backend loads its own init file: `init-s7.scm` or
    `init-guile.scm`.
  - That file loads the shared `init-kernel.scm` and `init-texmacs.scm`.
  - Shared code that has to differ tests `(s7-scheme?)`, so each
    interpreter runs the code it ran before.
- **s7 is faster than Guile 1.8.7 on everything measured:**
  - the first window is ready after about 0.7 s instead of 1.9 s, and
    startup, including the work deferred to idle moments, ends after about
    1.4 s instead of 3.7 s;
  - the regression suites run 1.4× faster;
  - repeated LaTeX export is 2.9× faster;
  - regenerating the manual is 1.25× faster;
  - s7 peaks at 20–40% more memory.

  See [07](07-performance.md).
- **Before merging** (see [06](06-open-issues.md#64-before-merging-upstream)):
  - build the configurations CI doesn't cover (MSVC, Android, CMake);
  - try plugins and user code written for Guile;
  - split the branch into a reviewable series;
  - report two s7 bugs upstream (both are worked around).
