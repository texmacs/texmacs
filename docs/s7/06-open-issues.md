# 6. Open issues, fragile spots and next steps

Status labels:

- **confirmed**: reproduced with the vendored s7, which I built standalone
  from `src/Scheme/S7/s7.c` with a 3-line driver, or evident from the code.
- **likely**: follows from reading the code, but not run inside TeXmacs.

## 6.1 Bugs

### 1. `ahash-size` returns the bucket count (confirmed, **fixed**)

`kernel/boot/ahash-table.scm` defines `(ahash-size h)` as `(length h)`. In s7,
that is `hash_table_mask + 1` (`s7.c:47968`).

- A fresh table gives `8`, and it still gives `8` after one insert. The entry
  count is `hash-table-entries`.
- The function has 34 call sites, including `(== (ahash-size h) 0)` tests
  and the change detection in `dynamic/calc-edit.scm`.
- `database/title-markup.scm` numbers entries with `(+ (ahash-size h) 1)`.

**Fixed:** `ahash-size` is now `hash-table-entries`.

### 2. `property` fails for procedure arguments (confirmed, **fixed**)

`procedure-name` now returns the procedure itself
(`tm-define.scm:91`). `property` does
`(if (procedure? var) (set! var (procedure-name var)))`, so it looks up the
key `(<procedure> . prop)`. `property-set!` stores the key under the symbol,
so the lookup never matches.

Callers that pass procedures:

- `compute-interactive-args` (`tm-dialogue.scm:237`): `(property fun :arguments)`.
  沈达's s7 fix (`b8315794fe`) used `procedure-symbol-name`, but it was
  replaced by the newer upstream version during the 2025 rebase.
  `tm-dialogue-test.scm` should catch this.
- `interactive-title` (`tm-dialogue.scm:279`): calls `(symbol->string name)`
  on a procedure when there is no `:synopsis`, which is an error.
- `procedure-sources` (`tm-define.scm:223`), `lazy-define-force` (`:367`)
  and `texmacs-mode-mode` (`tm-modes.scm:66`) also expect a symbol.

The same problem stopped `lazy-define-force` from forcing anything when
it was given a procedure. For example, `(lazy-define-force run-all-tests)`
left the check module unloaded.

**Fixed:** all five callers now use `procedure-symbol-name`. Its fallback
also changed. It used to accept a procedure's printed name only if that name
was entirely alphabetic, so `url-exists?` and `utf8->cork` failed. It now
accepts any printed name that does not start with `#<`, which is how s7
prints anonymous procedures. `procedure-name` itself is unchanged, so its
tests still pass.

### 3. `cite-sort-test` never loads (confirmed, **fixed**)

In `check/check-master.scm:15-26`, a misplaced parenthesis closes `:use`
before `(utils cite cite-sort-test)`. The `texmacs-module` macro in
`boot-s7.scm` turns any unknown option into `(noop)` without a warning, so
`(regtest-cite-sort)` ends up unbound when `run-all-tests` calls it.

**Fixed:** the parenthesis is moved. Not done yet: making `texmacs-module`
warn about unknown options, so that mistakes like this are reported.

### 4. The `catch` wrapper fails when the error has no data (confirmed, **fixed**)

The handler adapter in `init-texmacs-s7.scm:41-44` does `(caadr args)`. This
fails when the error info is not a pair. For example, `(throw 'foo)` with no
arguments raises a `wrong-type-arg` from inside the handler, and the original
error is lost.

No such `throw` exists in `progs` today.

**Fixed:** the adapter checks with `pair?` first. When the error carries no
data, it passes `""` as the message and the raw info as the arguments. The
output for errors that worked before is unchanged.

### 5. The apidoc source scanner uses Guile-only functions (likely)

`doc/apidoc-funcs.scm:107-118` (`parse-form`) uses `source-property` and
`def-keywords`. Neither is defined under s7: `def-keywords` exists only in
`init-texmacs.scm`. The "module exported symbols" part of the API docs should
fail.

### 6. Memo tables no longer cache `#f` (confirmed)

Storing `#f` in an s7 hash table does not create an entry. `logic-holds?`
(`logic-data.scm:34`) and `texmacs-submode?` (`tm-modes.scm:76`) use
`ahash-get-handle` specifically to remember negative answers, and now
recompute them on every call. This costs performance but does not give wrong
results.

**Fix:** store a sentinel value.

### 7. `prog-format` regression test fails (confirmed, pre-existing, not fixed)

`regtest-prog-format` fails on its first case: `*.cpp` is detected as
`"generic"` instead of `"cpp"`. It also fails without the fixes above.
Because `run-all-tests` stops at the first error, the suites that come after
it (`tm-define`, `tm-dialogue` and `cite-sort`) never run. For now, run those
suites one at a time.

### How the fixes were tested

Everything was run against the existing `TeXmacs/bin/texmacs.bin`. The
Scheme-only changes need no rebuild. The command was:

```
TEXMACS_HOME_PATH=<scratch> QT_QPA_PLATFORM=offscreen texmacs.bin -x '(load "probe.scm")'
```

The probe script runs each regression suite and each targeted probe inside
its own `catch`.

| Probe | Before | After |
|---|---|---|
| `regtest-tm-define` | not loadable (the `lazy-define-force` bug) | ok |
| `regtest-tm-dialogue` | not loadable | ok |
| `regtest-cite-sort` | unbound | ok |
| `ahash-size` of a 2-entry table | 8 | 2 |
| `(catch #t (λ () (throw 'foo)) …)` | `wrong-type-arg` in `caadr` | caught |
| `(interactive-title (lambda (x) x))` | `symbol->string` error | "Interactive command" |
| `(interactive-title system)` | `symbol->string` error | "Interactive command 'system'" |
| `(property detect-remote-plugins :arguments)` | `#f` | `(where)` |
| `(texmacs-mode-mode in-math?)` | hangs (infinite recursion; killed after 240 s) | `unknown%` |

## 6.2 Fragile or surprising behavior

- **`use-modules` copies values** (see §2.2). If a module later does a `set!`
  or redefines a `define-public` variable, modules that already imported it
  keep the old value. There is also no enforcement of `:use`: every module
  can see the whole top-level user environment.
- **`define-macro` is `define-expansion`.** Such macros expand at read time,
  so a macro must be defined before any file that uses it is read, and
  redefinitions do not affect code that was already read.
- **`ahash-get-handle` returns a fresh cons.** A `set-cdr!` on the handle
  does not write through to the table. No caller does this today.
- **`with-global` is not unwind-safe**: a non-local exit leaves the variable
  changed. The Guile version had the same problem.
- **`curried-define` exists only in `*texmacs-user-module*`.** Code evaluated
  in the rootlet cannot use `(define ((f a) b) …)`.
- **`assoc-set!` returns a new list** instead of mutating in place, and it is
  defined twice in `compat-s7.scm`.
- **Some compat functions are partial versions of their Guile originals:**
  - `string-index` and `string-rindex` take no start/end arguments;
  - `iota` takes only one argument;
  - `append!` and `delq` are non-destructive;
  - `lazy-catch` unwinds before running the handler.
- **Glue error messages are vague.** Argument errors say
  `"some other thing"` instead of the expected type (`TMSCM_ASSERT`).
- **`tmscm_install_procedure` ignores its optional and rest argument counts.**
- **Boot is noisy and has debugging code on by default.**
  `init-texmacs-s7.scm` does all of this on every start:
  - runs two fib benchmarks;
  - prints timings;
  - forces all lazy keyboard modules;
  - schedules `benchmark-menu-expand`;
  - hard-codes `developer-mode? #f`.

## 6.3 Guile leftovers

- **Build.** Guile is still required and linked. `init_guile` in
  `init_texmacs.cpp`, and `texmacs_init_guile_hooks` in the Unix, Windows64
  and Android entry points, still include Guile headers. CMake
  `SCHEME_IMPL=s7` is a stub. The glue generator runs under `guile`.
- **Scheme files that only work under Guile:**
  - `init-texmacs.scm`, `kernel/boot/boot.scm` and `kernel/boot/compat.scm`
    (only the Guile boot loads them);
  - `utils/misc/doxygen.scm:15` uses `(ice-9 rdelim)`;
  - the trace facility in `kernel/boot/debug.scm:284` uses
    `procedure-property`.
- **`$GUILE_LOAD_PATH`** is still used as the module search path.

## 6.4 Suggested next steps

1. Fix the `prog-format` regression (bug 7). Also make `run-all-tests`
   continue past a failing suite.
2. **Remove the need for Guile in the build.**
   - Add a `--with-scheme=s7|guile` option (and a real CMake `SCHEME_IMPL=s7`)
     that sets a `SCHEME_S7` or `SCHEME_GUILE` macro.
   - Use that macro in `object.hpp` and around the Guile hooks in the
     platform entry points.
   - Stop linking `-lguile`.
   - Port `build-glue` so it runs under s7. For example, add a small
     standalone s7 driver built from the vendored `s7.c`.
3. **Put the s7-specific code in one place again.**
   - Move s7-only idioms (`varlet`, `rootlet`, `hash-table-*`, `*s7*`) out of
     `ahash-table.scm`, `tm-define.scm` and `tm-modes.scm`.
   - Put them in `compat-s7.scm`, or in a matching `compat-guile.scm`.
   - The kernel would then work with both interpreters, as the README
     intended.
4. Generate `init-texmacs-s7.scm` from `init-texmacs.scm`, or share a common
   body, so the two do not drift apart. Commit `bbe7dfe2b9` exists only to
   resync them.
5. Put the benchmarks and `lazy-keyboard-force` behind a flag.
6. Refresh the vendored s7, re-applying `s7-lookup_from.patch`, and
   regenerate `s7.c.orig` from the same upstream revision so that the diff
   shows only the local patch.
