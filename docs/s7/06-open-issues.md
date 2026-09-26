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

**Fixed** (upstream fixed it the same way in the meantime). Not done yet:
making `texmacs-module` warn about unknown options, so that mistakes like
this are reported.

### 4. The `catch` wrapper fails when the error has no data (confirmed, **fixed**)

The handler adapter in `init-texmacs-s7.scm:41-44` (now `init-s7.scm`) did `(caadr args)`. This
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
`init-guile.scm`. The "module exported symbols" part of the API docs should
fail.

### 6. Memo tables no longer cache `#f` (confirmed)

Storing `#f` in an s7 hash table does not create an entry. `logic-holds?`
(`logic-data.scm:34`) and `texmacs-submode?` (`tm-modes.scm:76`) use
`ahash-get-handle` specifically to remember negative answers, and now
recompute them on every call. This costs performance but does not give wrong
results.

**Fix:** store a sentinel value.

### 7. `prog-format` regression test fails (confirmed, **fixed**)

`regtest-prog-format` failed on its first case: `*.cpp` was detected as
`"generic"` instead of `"cpp"`.

**Cause:** `init-texmacs-s7.scm` had drifted from `init-texmacs.scm`. It
still declared `(lazy-format (prog prog-format) cpp scheme scala java python)`,
while upstream had moved these languages to `code-format`, `python-format`
and similar modules. Other stale entries had the same origin:

- a broken `(when (url-exists? "") …)` guard around `init-user-shortcuts`;
- the missing `lazy-tool` declarations;
- the old `(various …)` module paths.

**Fixed:** during the rebase, the body of `init-texmacs-s7.scm` was
regenerated from the current `init-texmacs.scm`. Only the s7 prelude and the
s7 debug tail were kept.

Because `run-all-tests` stops at the first error, one failing suite hides
all the later ones. The probe script below runs each suite separately.

### 8. s7 optimizer mis-applies closures called from a loop (confirmed, upstream, worked around)

A loop that calls a closure argument can mis-apply a *different* closure
after it has run once with a closure of another shape. This reproduces with
the vendored s7 10.0, with the unpatched `s7.c.orig`, and with **s7 11.9
(21-Sep-2026)**, so it is not caused by the local patch and has not been
fixed upstream:

```scheme
(define (mk s) (let ((chars (string->list s))) (lambda (ch) (and (memv ch chars) #t))))
(define (inter . css) (lambda (ch) (let loop ((cl css)) (or (null? cl) (and ((car cl) ch) (loop (cdr cl)))))))
(define (count cs) (let loop ((i 0) (n 0)) (if (= i 256) n (loop (+ i 1) (if (cs (integer->char i)) (+ n 1) n)))))
(count (inter (mk "!?") (mk "aB!")))   ; => 1
(count (mk "abc"))                     ; => error: memv second argument, #\null, ... should be a list
```

The error shows that inside the `mk` closure, `chars` evaluates to the loop's
character.

- **Where it surfaced.** Upstream's `server-strong-password?` was written
  with SRFI-14 char-sets, and the first compat implementation of those used
  closures. The same pattern could affect any higher-order code, for example
  `string-index` with predicate arguments.
- **Workaround.** In `compat-s7.scm`, char-sets are now hash tables. They
  are still applicable, so `(cs ch)` works as before, but no closure is
  involved.

**To do:** report the bug to the s7 maintainer, using the reproduction above.
The same should be done for the loss of internal definitions of macro
bodies. A reproduction is the original `case-lambda` of `srfi.scm`, loaded
in a sublet whose `define` is `curried-define`, and used inside a function
body: `(let ((f (case-lambda ((x) 1) ((x y) 2)))) (f 1))` raises
`unbound variable alength`.

### 9. New upstream code needed s7 support (confirmed, **fixed**)

The 889 upstream commits pulled in by the rebase brought three new
Guile dependencies:

- **`TMSCM_ASSERT_UINT`** in `glue.cpp` called Guile's `scm_positive_p`,
  which broke the build. This affected `gnutls-random-number`.
- **SRFI-14 char-sets** in `server/server-authentication.scm`:
  `string->char-set`, `char-set-intersection`, `char-set-size`,
  `char-set:lower-case`, and others. Because of the failed load,
  `server-base` never defined `tm-service`, and every `server-*` module
  failed after it.
- **`(set! *random-state* …)`** in `server/server-base.scm`.

**Fixed:**

- The uint check is now interpreter-neutral, and `tmscm_to_uint` was added.
- The char-sets are the hash-table implementation from bug 8.
- `*random-state*` is a rootlet variable whose setter reseeds
  `(*s7* 'default-random-state)`.

A scan of the new upstream Scheme code for other Guile-only builtins found
nothing else. Checked, among others: `hash-ref`, `module-ref`, `ice-9`,
`getpwnam`, `source-property`, `string-contains`, `procedure-name`,
`string-join`, `append-map`, `every`, `last`.

### 10. `delay` did not memoize (confirmed, **fixed**, found by the new tests)

The compat `delay` stored `(lambda () expr)` as the value of an
already-forced promise, and `force` called it. As a result, every `force`
re-evaluated the expression. The result was still correct, but the work was
repeated, for example the closest-colour search in `convert/tools/tmcolor.scm`.

**Fixed:** `delay` and `force` now follow the R7RS reference implementation.

### 11. Mode predicates had no name (confirmed, **fixed**, found by the new tests)

Mode predicates such as `in-math?` are installed as anonymous lambdas, so
`procedure-symbol-name` returned `#f` for them, and
`(texmacs-mode-mode in-math?)` returned `unknown%`. On Guile it returned
`in-math%`.

**Fixed:** `texmacs-mode` now registers the name in `tm-defined-name`.

### 12. The move-to-front lookup patch was unsound (confirmed, **fixed**)

The local s7 patch reordered environments during lookups. That breaks
iteration over an environment, and possibly the positional refill of reused
argument lets. The patch is now an id check that never mutates. See
[05](05-build-and-history.md#s7-version-and-local-patch).

### 13. Regressions in shared C++ code (confirmed, **fixed**)

Found on 2026-09-26 by reviewing the port's changes outside `src/Scheme`:

- **`unescape_guile` broke Guile builds.** `analyze.cpp` skipped four
  characters after every `\xHH` escape, to fit s7's `\xHH;` format. On
  Guile, which prints `\xHH`, that dropped the character after each
  escape, in `object->tmstring`. The `;` is now skipped only on s7, and only
  when it is there. Two tests in `tm-glue-test.scm` cover both formats.
- **`qt_gui.cpp` reverted an upstream change.** The squashed port had turned
  the computation of the update delay back into an older `int` version.
  Upstream's `time_t` version is restored.
- **`windows64_system.cpp` lost its CRLF line endings,** which made a
  two-line change look like 1568. The line endings are restored.

### How the fixes were tested

The first round (bugs 1–4) ran against the July 2025 binary. After the
rebase, everything was rerun against a clean rebuild on
`svn_sync_20260921`. The command was:

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

Since 2026-09-24, most of these checks are also permanent regression tests
(see [04](04-progs-changes.md#44-tests-for-the-s7-port)). `run-all-tests` runs
564 tests in 17 suites and passes on s7 11.9.

Results after the rebase, on the clean rebuild:

- **Boot:** no Scheme errors.
- **`run-all-tests`:** all 11 suites pass, run one at a time: htmltm, xmltm,
  tmlength, environment, mathtm, tmhtml, tmmltm, prog-format, tm-define,
  tm-dialogue and cite-sort.
- **`run-integration-tests`:** all four suites pass: deletion-plan,
  server-notifications, server-backup and server-cache.
- **Probes:** all nine pass: the ones in the table above, plus
  `server-strong-password?`, `tm-service` defined, `gnutls-random-number`,
  `get-user-login` / `get-user-name`, and reseeding through
  `*random-state*`.

## 6.2 Fragile or surprising behavior

- **`use-modules` copies values** (see §2.2). If a module later does a `set!`
  or redefines a `define-public` variable, modules that already imported it
  keep the old value. There is also no enforcement of `:use`: every module
  can see the whole top-level user environment.
- **Macros are run-time macros** (s7 `define-macro`). They are expanded on
  every evaluation, like the effective behavior on s7 10 (see §2.1).
- **s7 11 quirks to keep in mind when writing kernel code:**
  - `varlet` refuses already-bound symbols in non-root lets.
  - Macro bodies should not define local helper functions; see
    [03](03-compat-layer.md).
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
- **`developer-mode?` is hard-coded to `#f`** in `init-s7.scm`. The
  boot-time benchmarks and the forced keyboard loading were removed on
  2026-09-24.

## 6.3 Guile leftovers

- **Build.** Fixed on 2026-09-24. The interpreter is a configure or CMake
  option, and an s7 build neither needs nor links Guile (see
  [05](05-build-and-history.md)). What remains are Guile-flavoured names: the
  function `init_guile` in `init_texmacs.cpp`, the empty
  `texmacs_init_guile_hooks` in s7 builds, and `$GUILE_LOAD_PATH` as the
  module search path.
- **Scheme files that only work under Guile:**
  - `init-guile.scm`, `kernel/boot/boot.scm` and `kernel/boot/compat.scm`
    (only the Guile boot loads them);
  - `utils/misc/doxygen.scm:15` uses `(ice-9 rdelim)`;
  - the trace facility in `kernel/boot/debug.scm:284` uses
    `procedure-property`.
- **`$GUILE_LOAD_PATH`** is still used as the module search path.

## 6.4 Suggested next steps

1. Make `run-all-tests` continue past a failing suite, and report the s7
   optimizer bug (bug 8) upstream.
2. **Test both interpreters routinely.** The Guile build boots and passes
   the tests (see [04](04-progs-changes.md), §4.2). New shared code, and
   future upstream syncs, should be checked with both builds, in particular
   for s7 reader syntax in shared files.
3. **Done on 2026-09-26: the two init files share one body.**
   - The C++ backend loads its own `init-s7.scm` or `init-guile.scm` (from
     `scheme_init_file ()`).
   - That file sets up its interpreter, then loads the common
     `init-kernel.scm` and `init-texmacs.scm`, with its own steps in
     between.
   - `init-texmacs-s7.scm` is gone. Drift between the two copies had caused
     one test failure (bug 7).
4. **Look at the Qt part of the boot** (see
   [05](05-build-and-history.md#boot-time)). It now dominates, and a Guile
   build pays it too.
5. Refresh the vendored s7, re-applying `s7-lookup_from.patch`, and
   regenerate `s7.c.orig` from the same upstream revision so that the diff
   shows only the local patch.
