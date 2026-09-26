# 4. Shared Scheme code and tests

## 4.1 One kernel for both interpreters

The Scheme code in `TeXmacs/progs` runs on both s7 and Guile. Each module
system defines `(s7-scheme?)`: `boot-s7.scm` returns `#t`, and Guile's
`boot.scm` returns `#f`. Code that has to differ tests it in one of two ways.

- **At expansion time, in the macros that generate definitions.**
  `tm-define`, `tm-define-macro`, `lazy-define` (`tm-define.scm`) and
  `texmacs-modes` (`tm-modes.scm`) emit either:
  - the s7 code: `varlet (rootlet)`, `eval` in the user module, environment
    lookup;
  - or upstream's Guile code: the `set-current-module texmacs-user`
    sequence, `module-ref`.

  Each interpreter runs the code it ran before.
- **At load time, around definitions.** These keep an s7 variant and
  upstream's Guile variant side by side:
  - `ahash-table.scm`: the hash-table primitives;
  - `abbrevs.scm`: `when`/`unless` for Guile, and `save-object`;
  - `prologue.scm`: `module-load` for Guile;
  - `list.scm`: `list-head`/`list-tail` for s7;
  - `regexp-select.scm`: `select`;
  - `debug.scm`: `scm-error*`;
  - `tm-define.scm`: `procedure-name`;
  - `tm-file-system.scm`: `object->tmstring`;
  - `environment.scm`, `scheme-autocomplete.scm`.

Some code simply works in both:
- `receive` uses `call-with-values`;
- the helpers of `case-lambda`, `and-let*`, `regression-test-group`,
  `trace-variables` and `kbd-symbols` are module-level functions or
  `let`-bound lambdas, not internal `define`s (see §3.2).

**Files used by only one interpreter:**

| s7 | Guile |
|---|---|
| `init-s7.scm` | `init-guile.scm` |
| `kernel/boot/boot-s7.scm` | `kernel/boot/boot.scm` |
| `kernel/boot/compat-s7.scm` | `kernel/boot/compat.scm` (also defines `tm-eval`, which C++ `eval (object)` calls) |
| tests `compat-s7-test`, `boot-s7-test` | |

`init-kernel.scm` and `init-texmacs.scm` are shared (§2.1).

**Shared files must stay readable by both readers.** In particular, s7 reader
syntax such as `#_define` can't appear in them.

## 4.2 Catalogue of s7-motivated edits

| File | Change |
|---|---|
| `kernel/boot/ahash-table.scm` | The hash-table primitives use s7's `hash-table-ref`/`hash-table-set!`. `ahash-remove!` stores `#f`, `ahash-table->list` is `(map values h)`, and `ahash-size` is `hash-table-entries` (`(length h)` is the bucket count in s7). |
| `kernel/boot/prologue.scm` | The Guile `module-load`, `list->module` and `module-loaded-table` live in `boot-s7.scm` on s7. |
| `kernel/boot/abbrevs.scm` | No `when`/`unless` macros (built-in syntax in s7). `with-global` is multiple-value safe via `call-with-values`. `save-object`/`load-object` use `call-with-output-file`/`call-with-input-file` with a raised `print-length`. |
| `kernel/boot/srfi.scm` | `receive` relies on values splicing. `cute` uses `delq` instead of `delq!`. |
| `kernel/boot/debug.scm` | No Guile `old-format?` probe. `scm-error*` is `(apply error …)`. |
| `kernel/library/list.scm` | Public `list-head` and `list-tail`. `any1` and `every1` are written without named-let loops. |
| `kernel/logic/logic-rules.scm` | `(logic-rules-version)` counts the rules added, for caches of query results. |
| `kernel/texmacs/tm-define.scm` | Definitions in the rootlet, `tm-define-macro` through `eval`, `procedure-name` / `procedure-symbol-name`, `lazy-define` through the environment (§2.5). |
| `kernel/texmacs/tm-modes.scm` | Mode predicates are installed with `varlet *texmacs-module*`, and their names are registered. |
| `kernel/texmacs/tm-file-system.scm` | `object->tmstring` raises `(*s7* 'print-length)`. |
| `kernel/texmacs/tm-dialogue.scm` | `tm-interactive-hook`; the interactive helpers use `procedure-symbol-name`. |
| `kernel/texmacs/tm-plugins.scm` | `plugin-configure-cmd` returns `#t` for `:macpath` and `:winpath`. |
| `kernel/regexp/regexp-select.scm` | `select` is rebound in the rootlet. |
| `graphics/graphics-utils.scm` | `define-macro` uses the `(name . args)` form. |
| `prog/scheme-autocomplete.scm` | `all-used-modules` and `all-used-symbols` walk `*modules*` and `tm-defined-table` instead of Guile obarrays. |
| `prog/scheme-tools.scm` | `string-rindex` without start/end arguments. |
| `generic/generic-edit.scm`, `version/version-tmfs.scm` | No Guile `string-contains`. |
| `database/db-users.scm` | The `get-user-login` and `get-user-name` glue instead of `getpwnam`/`getlogin`. |
| `convert/latex/tmtex.scm` | No `(ice-9 format)`. |
| `convert/latex/latex-tools.scm` | `latex-needs?` caches its answers until logic rules are added. |
| `convert/html/tmhtml.scm` | Percentages are rounded explicitly (s7 prints floats with more digits); ornament keywords are recognised with `keyword?`, not by their printed form. |
| `convert/tools/environment.scm` | `environment-ref*` is a bare `ahash-ref`. |
| `source/macro-widgets.scm` | Guards against `(string->symbol "")`. |
| `utils/automate/auto-build.scm` | `auto-safe-mode?` is a `tm-define`, so it is visible from the rootlet. |
| `convert/{html,tools}/*-test.scm` | Tests that relied on Guile behavior (empty PI symbol, unbound-variable errors) are disabled. |

## 4.3 Fixes to shared code made on this branch

These fixes change behavior on both interpreters, or fix upstream code that
did not work on s7.

| Fix | Where |
|---|---|
| `ahash-size` returned the bucket count on s7 | `ahash-table.scm` |
| `property`, `lazy-define-force`, `compute-interactive-args`, `interactive-title` and `procedure-sources` failed when given a procedure; they now use `procedure-symbol-name` | `tm-define.scm`, `tm-dialogue.scm`, `tm-modes.scm` |
| The `catch` adapter failed on errors without data | `init-s7.scm` |
| A misplaced parenthesis kept `cite-sort-test` from loading (fixed upstream too) | `check-master.scm` |
| `delay` did not memoize | `compat-s7.scm` |
| Mode predicates had no name, so `texmacs-mode-mode` failed | `tm-modes.scm` |
| New upstream server code used SRFI-14 char-sets and `*random-state*` | `compat-s7.scm` |
| The HTML export used `hash-map->list` and `string-prefix?`, and printed keywords | `compat-s7.scm`, `tmhtml.scm` |
| `latex-needs?` ran a logic query for every document node | `latex-tools.scm`, `logic-rules.scm` |

## 4.4 Tests

The regression suites are listed in `check/check-master.scm`, and
`run-all-tests` runs them in order. The suites written for the port run first:

| Suite | Module | Tests | What it checks |
|---|---|---|---|
| `regtest-compat-s7` | `kernel/boot/compat-s7-test` | 91 | The compatibility layer: lists and alists, `while`, the `symbol?`/`list?` rebindings, string functions, char-sets (including the s7 closure bug), records, `delay`/`force`, `hash`, `*random-state*`, curried `define`, `stable-sort`, `hash-map->list` |
| `regtest-boot-s7` | `kernel/boot/boot-s7-test` | 56 | The reader (`'x` as `(quote x)`, quasiquote, multiple values, NUL in strings), the `catch` adapter, run-time macros, the module system (exports, private definitions, `use-modules`, `inherit-modules`, publication in the rootlet, `tm-define-macro`), and lookups in large environments (iteration and reused argument lets) |
| `regtest-abbrevs` | `kernel/boot/abbrevs-test` | 60 | Adaptive hash tables, programming constructs (`with`, `with-global`, `for`, …), SRFI macros (`receive`, `case-lambda`, `cut`, `and-let*`), `save-object`/`load-object` |
| `regtest-logic` | `kernel/logic/logic-engine-test` | 21 | Unification, logic tables with run-time names, dispatchers, groups, rules and queries with free variables, `logic-rules-version` |
| `regtest-tm-glue` | `kernel/texmacs/tm-glue-test` | 41 | C++/Scheme conversions: booleans, integers, doubles, strings with NUL, UTF-8 and escapes, string arrays, trees, urls, commands from closures, blackboxes across `gc` |
| `regtest-tm-define` | `kernel/texmacs/tm-define-test` | 34 | Procedure names, overloading with `:require` and `former`, properties, `tm-property`, the interactive helpers, `tm-define-macro`, mode predicates |
| `regtest-tm-dialogue` | `kernel/texmacs/tm-dialogue-test` | 2 | `compute-interactive-args` |
| `regtest-tm-convert` | `kernel/texmacs/tm-convert-test` | 25 | Format registration |

The other seven suites are upstream's: `htmltm`, `xmltm`, `tmlength`,
`environment`, `tmhtml`, `prog-format` and `cite-sort`. All 15 suites except
the two s7-specific ones also run on Guile.

`kernel/logic/logic-test.scm` is not a test suite. It holds Joris van der
Hoeven's examples of logical programs, which is why the logic tests live in
`logic-engine-test.scm`.

### Running the tests

```
TEXMACS_HOME_PATH=<scratch dir> QT_QPA_PLATFORM=offscreen \
  TeXmacs/bin/texmacs.bin -x '(begin (run-all-tests) (quit-TeXmacs))'
```

- **One suite:** `(use-modules (kernel boot boot-s7-test))`, then
  `(regtest-boot-s7)`.
- **Server integration tests:** `(run-integration-tests)`.
- **Use a scratch home directory.** TeXmacs runs its first-start setup in
  the home directory it is given, so a scratch `TEXMACS_HOME_PATH` keeps your
  real one untouched.
- **A failing test stops `run-all-tests`.** The report shows the expected
  and actual values. `docs/s7/bench/suites.scm` runs the portable suites one
  by one, each in its own `catch`.

### Things to know when writing tests

- **Each expression is evaluated twice.** `regression-test-group` evaluates
  every test expression once for the "Result in" display and once for the
  comparison. Tests with side effects must be idempotent.
- **Test modules run in module environments.** `define` there is
  `curried-define`, and `tm-define` definitions are global, so give them
  distinctive names.

The C++ tests in `tests/` do not exercise Scheme.
