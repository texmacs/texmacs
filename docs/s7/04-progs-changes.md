# 4. Changes to shared Scheme modules (`TeXmacs/progs`)

## 4.1 Most of the diff is not s7 work

`git diff master...HEAD -- TeXmacs/progs` touches 158 files. **Most of those
changes are not s7-related.** The local `master` is at the merge-base
`631aa5c679` (Jan 2023), and `wip_s7` was rebased onto 2025 upstream, so the
diff also contains upstream work. That upstream work includes:

- side and bottom tools;
- the markup-driven GUI (`kernel/gui/menu-convert.scm`, `menu-widget.scm`,
  `utils/misc/gui-utils.scm`, and the `*-widgets.scm` / `*-tools.scm`
  rewrites);
- Qt, Android and converter fixes.

None of the plugin `progs` changes are s7-specific.

The s7 changes to shared files come mostly from `29481b3441` ("S7 support
imported from mgubi/texmacs") and a few follow-up commits (see
[05](05-build-and-history.md)).

## 4.2 The kernel is now s7-only

**Nothing detects the interpreter at the Scheme level**: there is no
`cond-expand`, no `*features*` check and no `(provided? 's7)`. The places
where dialect-dependent code remains are:

- `kernel/boot/boot.scm:19-23`: `guile-a?`, `guile-b?` and `guile-c?`.
  This file is loaded only by the Guile boot.
- `init-texmacs.scm:63`: Guile-only.
- `doc/apidoc-funcs.scm:70`: a `(guile?)` helper that nothing calls.
- `convert/images/tmimage.scm:42`: an upstream feature test,
  `(if (not (defined? 'string-contains)) …) ; for s7`.

The kernel modules below were rewritten in place with no guard, so the tree
can no longer boot on Guile even though `init-texmacs.scm` is still there.

## 4.3 Catalogue of s7-motivated edits

| File | Change |
|---|---|
| `kernel/boot/ahash-table.scm` | Both Guile implementations (vector+count, and the `hash-ref` aliases) were replaced by s7's `hash-table-ref`/`hash-table-set!`. `ahash-remove!` stores `#f`, `ahash-table->list` is `(map values h)`, and `ahash-size` is `(length h)` (**bug**, see 06). `ahash-fold` was dropped because nothing used it. |
| `kernel/boot/prologue.scm` | The Guile `module-load`, `list->module` and `module-loaded-table` were removed (they now live in `boot-s7.scm`). |
| `kernel/boot/abbrevs.scm` | Removed the `when`/`unless` macros (built-in syntax in s7). `with-global` is now multiple-value safe via `call-with-values`. `save-object`/`load-object` now use `call-with-output-file`/`call-with-input-file` with a raised `print-length`, replacing `open-file`, `pretty-print` and `flush-all-ports`. |
| `kernel/boot/srfi.scm` | `receive` relies on values splicing. `cute` uses `delq` instead of `delq!`. |
| `kernel/boot/debug.scm` | Removed the Guile `old-format?` probe. `scm-error*` is now `(apply error …)`. |
| `kernel/library/list.scm` | Adds public `list-head` and `list-tail`. `any1` and `every1` are rewritten without named-let loops. |
| `kernel/texmacs/tm-define.scm` | Module-free definitions via `varlet (rootlet)`, `procedure-name` / `procedure-symbol-name`, `:synopsis*`, and a `lazy-define` lookup through the environment. See §2.3. |
| `kernel/texmacs/tm-modes.scm` | Mode predicates are installed with `varlet *texmacs-module*`. |
| `kernel/texmacs/tm-file-system.scm` | `object->tmstring` raises `(*s7* 'print-length)`. |
| `kernel/texmacs/tm-dialogue.scm` | Uses `tm-interactive-hook` (defined in both init files). The s7 fix to `compute-interactive-args` was lost in the rebase (see 06). |
| `kernel/texmacs/tm-plugins.scm` | `plugin-configure-cmd` returns `#t` for `:macpath` and `:winpath`. |
| `kernel/regexp/regexp-select.scm` | Replaced the Guile arity-dispatching `select` with `(varlet *texmacs-module* 'select tm-select)`. |
| `graphics/graphics-utils.scm` | `define-macro` now uses the `(name . args)` form. |
| `prog/scheme-autocomplete.scm` | `all-used-modules` and `all-used-symbols` walk `*modules*` and `tm-defined-table` instead of Guile obarrays. A FIXME says this belongs in the compat layer. |
| `prog/scheme-tools.scm` | `string-rindex` no longer takes start/end arguments; a `substring` is used instead. |
| `generic/generic-edit.scm`, `version/version-tmfs.scm` | Guile's `string-contains` was replaced (commit `441a842861`). |
| `database/db-users.scm` | Uses the `get-user-login` and `get-user-name` glue instead of `getpwnam`/`getlogin`. |
| `convert/latex/tmtex.scm` | The `(use-modules (ice-9 format))` line is commented out. |
| `convert/html/tmhtml.scm` | Percentages are rounded explicitly, because s7 prints floats with more digits. |
| `convert/tools/environment.scm` | `environment-ref*` is a bare `ahash-ref`, so the unbound-key warning is gone. |
| `source/macro-widgets.scm` | Guards against `(string->symbol "")`. |
| `utils/automate/auto-build.scm` | `auto-safe-mode?` is now a `tm-define`, so it is visible from the rootlet. |
| `convert/{html,tools}/*-test.scm` | Disabled tests that relied on Guile behavior (empty PI symbol, unbound-variable errors). |

## 4.4 Tests for the s7 port

The regression suites are listed in `check/check-master.scm`, and
`run-all-tests` runs them in order. The suites specific to the port run first:

| Suite | Module | Tests | What it checks |
|---|---|---|---|
| `regtest-compat-s7` | `kernel/boot/compat-s7-test` | 77 | Guile compatibility layer: lists and alists, `while`, `symbol?`/`list?` rebindings, string search, char-sets (including the s7 closure bug), records, `delay`/`force`, `hash`, `*random-state*`, curried `define` |
| `regtest-boot-s7` | `kernel/boot/boot-s7-test` | 50 | Reader (`'x` as `(quote x)`, quasiquote, multiple values, NUL in strings), the `catch` adapter, run-time macros (not expanded in quasiquoted templates, helper definitions in macro bodies), the module system (exports, private definitions, repeated `use-modules`, `inherit-modules`), lookup in large lets (the local s7 patch) |
| `regtest-abbrevs` | `kernel/boot/abbrevs-test` | 60 | Adaptive hash tables, programming constructs (`with`, `with-global`, `for`, …), SRFI macros (`receive`, `case-lambda`, `cut`, `and-let*`), `save-object`/`load-object` |
| `regtest-logic` | `kernel/logic/logic-engine-test` | 19 | Unification, logic tables with run-time names, dispatchers, groups, rules and queries with free variables |
| `regtest-tm-glue` | `kernel/texmacs/tm-glue-test` | 39 | C++/Scheme conversions: booleans, integers, doubles, strings with NUL and UTF-8, string arrays, trees, urls, commands from closures, blackboxes across `gc` |
| `regtest-tm-define` | `kernel/texmacs/tm-define-test` | 34 | Procedure names, overloading with `:require` and `former`, properties, `tm-property`, `compute-interactive-args`, `interactive-title`, `tm-define-macro`, mode predicates |
| `regtest-tm-dialogue` | `kernel/texmacs/tm-dialogue-test` | 2 | `compute-interactive-args` |
| `regtest-tm-convert` | `kernel/texmacs/tm-convert-test` | 25 | Format registration (existing suite, now wired in) |

`kernel/logic/logic-test.scm` is not a test suite. It holds Joris van der
Hoeven's examples of logical programs, which is why the logic tests live in
`logic-engine-test.scm`.

### Running the tests

```
TEXMACS_HOME_PATH=<scratch dir> QT_QPA_PLATFORM=offscreen \
  TeXmacs/bin/texmacs.bin -x '(begin (run-all-tests) (quit-TeXmacs))'
```

- **One suite:** `(use-modules (kernel boot boot-s7-test))` and then
  `(regtest-boot-s7)`.
- **Server integration tests:** `(run-integration-tests)`.
- **Scratch home directory.** TeXmacs runs its first-start setup in the home
  directory it is given, so a scratch `TEXMACS_HOME_PATH` keeps your real one
  untouched.
- **Failures.** A failing test stops `run-all-tests`. The report shows the
  expected and actual values.

### Things to know when writing tests

- **Each expression is evaluated twice.** `regression-test-group` evaluates
  every test expression once for the "Result in" display and once for the
  comparison. Tests with side effects must be idempotent, e.g. use `gensym`
  instead of a fixed new symbol.
- **Test modules run in module environments.** `define` there is
  `curried-define`, and `tm-define` definitions are global, so prefix their
  names (e.g. `tmdt-`).

The C++ tests in `tests/` do not exercise Scheme.
