# 3. The compatibility layer and Guile/s7 differences

The compatibility layer is `TeXmacs/progs/kernel/boot/compat-s7.scm`. It is the
first module inherited at boot. In addition, `s7_tm.cpp` defines
`current-time` and `getpid` in C.

## 3.1 What `compat-s7.scm` provides

| Group | Definitions | Notes |
|---|---|---|
| SRFI-1 / Guile list functions | `filter` (only if missing), `delq` (non-destructive), `acons`, `last-pair`, `list-copy` (via generic `copy`), `copy-tree`, `map-in-order` (= `map`), `append!` (= non-destructive `append`) | |
| Association lists | `assoc-ref`, `assoc-set!` | `assoc-set!` is defined twice (identical copies) and returns the new list. Guile's version mutates in place, so callers must use the return value. |
| Arithmetic and misc | `1+`, `1-` (as macros), `noop`, `symbol-append`, `seed->random-state`, `*random-state*`, `force-output`, `iota` (single-argument form only) | `*random-state*` lives in the rootlet. Its setter reseeds `(*s7* 'default-random-state)`. |
| Strings | `string-null?`, `string-split` (char separator), `string-index` / `string-rindex` (char, char-set or predicate, no start/end) | |
| Char-sets (SRFI-14 subset) | `char-set`, `string->char-set`, `char-set-adjoin`, `char-set-complement`, `char-set-intersection`, `char-set-union`, `char-set-contains?`, `char-set-size`, `char-set:whitespace`, `char-set:lower-case`, `char-set:upper-case`, `char-set:digit` | Char-sets are hash tables mapping characters to `#t`. They are applicable, so `(cs ch)` tests membership. Predicates are accepted wherever a char-set is expected. Closures are avoided because of an s7 optimizer bug (06, bug 8). |
| Sorting | `(sort l op)` = `(sort! (copy l) op)` | s7's `sort!` is destructive. |
| Errors | `lazy-catch` = `catch` | Guile's `lazy-catch` runs the handler before unwinding; here it unwinds first. |
| Records | `make-record-type`, `record-constructor`, `record-accessor` (a macro), `record-predicate` | Records are `inlet`s with a `'type` slot. The constructor is built with `eval`. |
| Promises | `make-promise`, `delay`, `delay-force`, `force` | Taken from s7's `r7rs.scm`. |
| Hashing | `(hash obj [bound])` | `(modulo (hash-code obj) bound)` |
| Loops | `while` | Uses `call-with-exit`. `break` and `continue` are bound inside the body. |
| `define` | `curried-define` | Installed as `define` **only** in `*texmacs-user-module*`, so module files (whose parent is that environment) can use `(define ((f a) b) …)`. Code evaluated in the rootlet cannot. |

The file ends with a "TODO/FIXME" about redefining `error` to match Guile's
calling convention. That has not been done.

## 3.2 Semantic differences between Guile and s7

These are the differences that forced changes elsewhere, or that code
reviewers should know about. The ones marked ✓ were checked against the
vendored s7: first 10.0 (11-Jan-2022), and again after the update to 11.9
(21-Sep-2026).

- **Keywords.** In s7, `:foo` is a symbol (and self-evaluating).
  `init-texmacs-s7.scm` rebinds `symbol?` to reject keywords.
- **Macros.** TeXmacs uses s7's native run-time `define-macro` (see §2.1).
  Guile's `(define-macro name (lambda args …))` form is not accepted, which
  is why `graphics-utils.scm` was rewritten.
- **Quote** ✓. By default s7 11 reads `'x` as `(#_quote x)`, where the car is
  the builtin `quote` object and not the symbol. TeXmacs turns on
  `(*s7* 'symbol-quote?)` to get standard Scheme reading.
- **`varlet`** ✓. In s7 11, `varlet` on a symbol already bound in the
  target (non-root) let raises "duplicate identifier". Use `let-set!` for
  existing bindings, as `import-bindings!` does. On the rootlet, `varlet`
  overwrites.
- **Macros with internal definitions** ✓. In s7 11, a macro whose body
  defines helper functions can lose them between recursive calls of those
  helpers. Seen with `case-lambda` used inside a function body with two or
  more clauses: `unbound variable alength`.
  - In standalone s7 it reproduces only when the internal `define` is
    TeXmacs's `curried-define` macro.
  - Inside TeXmacs it also happened with the builtin `#_define`.
  - The helpers of `case-lambda` are now module-level functions.
  - The other macros with internal definitions (`and-let*`,
    `regression-test-group`, `trace-variables`, `kbd-symbols`,
    `texmacs-module`) use the builtin `#_define`, so they do not go through
    `curried-define`.
- **Multiple values splice** ✓. `(+ 1 (values 2 3))` is `6`, and
  `(values)` disappears from an argument list.
  - `receive` in `srfi.scm` now relies on this:
    `((lambda vars body) vals)`.
  - `with-global` in `abbrevs.scm` uses `call-with-values` so that it works
    with both interpreters.
  - Some code relies on `(values)` to drop elements inside `map`, for example
    in `use-modules` and `scheme-autocomplete.scm`.
- **`list?`** is true for any pair, dotted or circular. It is rebound to
  `proper-list?`.
- **Hash tables.**
  - `(length ht)` is the **bucket count**, not the number of entries ✓. For
    a fresh table it is 8, and it stays 8 after one insert. The entry count
    is `hash-table-entries`.
  - Storing `#f` removes the key, or does not add it ✓.
  - `map` or `for-each` over a table yields `(key . value)` pairs.
  - Tables and environments are applicable: `(ht key)`, `(env 'sym)`.
- **`(string->symbol "")` is an error** ✓ (`wrong-type-arg`). Guile allows
  it. Guards were added in `macro-widgets.scm`, and one `xmltm` test was
  disabled.
- **Procedure printing** ✓. `(object->string car)` → `"car"`, and a named
  closure prints as its name. `procedure-symbol-name` relies on this.
- **Print length** ✓. `*s7* 'print-length` (default 12) truncates vectors,
  hash tables and similar, with `...`. `save-object` and `object->tmstring`
  raise it temporarily with `let-temporarily`. Plain lists are not truncated.
- **Missing builtins** that callers had to replace:
  - `string-contains`: replaced by `string-search-forwards`,
    `string-contains?` or `string-position`.
  - `getpwnam`, `getlogin`, `passwd:gecos`: replaced by the C++ glue
    functions `get-user-login` and `get-user-name`.
  - `(ice-9 format)`: s7 has a built-in `format`.
  - `list-head`: now defined in `kernel/library/list.scm`.
  - `procedure-name`, `source-property`, `symbol-property`: see §2.3 and
    [06](06-open-issues.md).
- **Numbers.** `round` returns an exact integer, so `(/ (round x) 100)` is
  a rational. s7 also prints floats differently from Guile, which is why
  `tmhtml.scm` now rounds percentages explicitly before printing.
- **Error handlers.** s7 calls a `catch` handler as `(type info)`, where
  `info` is usually `(format-string . args)`. The `catch` wrapper in
  `init-texmacs-s7.scm` converts this to Guile's shape, `(key subr msg args)`.
- **Evaluation environment.** A bare `eval` uses the current `curlet`, and
  `load` uses the rootlet. Both are wrapped (§2.1).
