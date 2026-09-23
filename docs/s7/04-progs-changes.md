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

## 4.4 Tests added for s7

- `kernel/texmacs/tm-define-test.scm`: `procedure-name` and
  `procedure-symbol-name`, including builtins, tm-defined procedures and
  anonymous lambdas.
- `kernel/texmacs/tm-dialogue-test.scm`: `compute-interactive-args` on a glue
  procedure and on a tm-defined one.

Both run through `check/check-master.scm`. To run the whole suite, use
`texmacs -x "(run-all-tests)" -q`. The C++ tests in `tests/` do not exercise
Scheme.
