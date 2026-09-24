# 2. Boot sequence and the module system

## 2.1 Boot order

1. **C++ prelude.** `start_scheme` → `initialize_scheme` evaluates a small
   prelude in the rootlet and registers the blackbox type and the glue
   (see [01](01-cpp-binding.md)).
2. **Init file.** `tm_server_rep::tm_server_rep` loads
   `$TEXMACS_PATH/progs/init-texmacs-s7.scm` into `user_env`
   (`src/Texmacs/Server/tm_server.cpp:138`). The Guile file
   `init-texmacs.scm` is still in the tree, is not used, and has to be kept in
   sync with the s7 file by hand.
3. **Rebindings.** The top of `init-texmacs-s7.scm` installs these:
   - **`quote` is read as the symbol `quote`.** `start_scheme` in
     `s7_tm.cpp` sets `(*s7* 'symbol-quote?)` to `#t`. Otherwise s7 11 reads
     `'x` as `(#_quote x)`, and TeXmacs code that inspects quoted forms, such
     as `(== (car x) 'quote)` in the logic engine, silently fails.
   - **`define-macro` is s7's native, run-time macro.** Until September 2026
     it was aliased to `define-expansion` (read-time macros). That never
     really worked:
     - **s7 10.** The reader did not see expansions defined outside the
       rootlet, which covers almost all TeXmacs macros. So they ran as
       run-time macros anyway.
     - **s7 11.** The reader does see them, and then also expands them
       inside quasiquoted templates, e.g. `` `($texmacs-output ,@l) ``. That
       breaks the menu code.

     Run-time macros are also what makes loading modules at macro-expansion
     time safe (see `inherit-modules` below).
   - **`symbol?` excludes keywords.** In s7, `:foo` is a symbol and satisfies
     `symbol?`. In Guile it does not. `primitive-symbol?` keeps the original.
   - **`*current-module*`** is set to the current environment (`user_env`),
     and `tm-eval` evaluates in `*texmacs-user-module*`.
   - **`load` and `eval`** are wrapped so that they default to
     `*current-module*` instead of s7's rootlet and curlet.
   - **`catch`** is wrapped so that handlers get Guile's 4-argument shape:
     `(key subr message args)`. `subr` is always `"[not-implemented]"`; the
     other values are taken from s7's `(type info)`. See
     [06](06-open-issues.md) for the case where this wrapper fails.
4. **Module system.** `kernel/boot/boot-s7.scm` is loaded (see §2.2), then the
   kernel modules are brought in with `inherit-modules`, starting with
   `(kernel boot compat-s7)` ([03](03-compat-layer.md)).
5. **The rest of the init file** mirrors `init-texmacs.scm` (`lazy-define`,
   `lazy-menu`, `lazy-keyboard`, …) with these differences:
   - `developer-mode?` is hard-coded to `#f`. The Guile version reads the
     `developer tool` preference.
   - The Guile reader hook that records source locations of definitions
     (`new-read`, `source-property`, `def-keywords`) is gone.
   - The Guile stack-size settings are gone.
   - Two developer commands are defined but not run:
     `(benchmark-menu-expand)` and `(benchmark-manual)`. You can run the
     second with `texmacs.bin -x "(benchmark-manual)"`.
   - **Until 2026-09-24 the file also did debugging work on every boot.** It
     ran two fib benchmarks, forced the loading of all lazy keyboard modules
     with `(lazy-keyboard-force #t)`, and scheduled `benchmark-menu-expand`.
     This took about 0.3 s, a third of the boot (see
     [05](05-build-and-history.md#boot-time)).
     - Forcing the keyboards is not needed: every `lazy-keyboard` form already
       schedules the module to be loaded at the first idle moment, and key
       handling loads the modules of the current mode on demand, as in the
       Guile init.

## 2.2 Modules as environments (`boot-s7.scm`)

Guile's module objects are replaced by s7 first-class environments (`let`s).

```
(rootlet) = *texmacs-module*      ← glue, compat, all tm-define'd symbols, mode predicates
   └─ user_env = *texmacs-user-module*   ← target of use-modules at toplevel
         └─ (sublet user '*exports* () '*module-file* …) — one per loaded module
```

### Globals

Defined in the rootlet:

- `*texmacs-module*`: the rootlet itself.
- `*module-name*`.
- `*modules*`: a hash table that maps module names such as
  `(kernel boot abbrevs)` to their environments.

### `texmacs-module name opts…`

This macro expands to the following, in the module's own environment:

- `(define *module-name* 'name)`
- `(define *exports* ())`
- a registration of `(current-module)` in `*modules*`
- `:use` → `use-modules`; `:inherit` → `inherit-modules`

`:export` prints a deprecation warning. Any other option is silently turned
into `(noop)`. That is how the parenthesis bug in `check-master.scm` goes
unnoticed (see [06](06-open-issues.md)).

### `module-load`

1. Resolves the file through `$GUILE_LOAD_PATH` (the variable name is
   historical).
2. Creates a fresh `sublet` of `*texmacs-user-module*` for the module.
3. Loads the file inside `with-module`. `with-module` uses `let-temporarily`
   so that the rootlet's `*current-module*` is the new environment while the
   file loads.

### Exports and imports

- **`define-public`, `define-public-macro`, `export`.** A normal `define`
  (in the module's environment), plus a push of the symbol onto the module's
  `*exports*`.
- **`use-modules m…`.** For each module:
  1. Resolve it, loading it if needed.
  2. Walk the module's environment. In s7, `map` over a `let` yields
     `(sym . val)` pairs.
  3. Keep the pairs whose symbol is in `*exports*`.
  4. Install them in the rootlet's `*current-module*`, which is whichever
     module is loading at that moment. `import-bindings!` does this. For a
     symbol that is already bound there, it updates the existing binding
     with `let-set!`, and it only `varlet`s new symbols. s7 11 refuses to
     `varlet` a symbol that is already bound in the target let ("duplicate
     identifier"). s7 10 used to add a shadowing slot instead.
- **`inherit-modules`.** `use-modules` plus re-exporting the imported
  modules' exports. `re-export-modules!` collects those exports when the form
  is evaluated, not when it is expanded. The old version resolved, and so
  loaded, the modules at expansion time. With read-time macros on s7 11, a
  `load` during an expansion silently ended the load of the file being read,
  and the rest of `init-texmacs-s7.scm` just vanished.
- **`import-from`.** An alias for `use-modules`.

### Consequences

- **Imports copy values.** `use-modules` copies the current value of each
  exported binding into the importer. If the exporting module later does a
  `set!` or redefines a `define-public` variable, importers keep the old
  value. Guile modules share the variable instead.
  - This matters little in practice, because the definitions most often
    overridden are made with `tm-define`, and those live in the rootlet
    (§2.3).
- **Modules see all of the user environment.** Every module's parent is
  `*texmacs-user-module*`, so every module can see everything imported at top
  level, not just what it declared with `:use`. Missing `:use` clauses are
  therefore not detected.
- **The top-level environment is very large.** Modules loaded from
  `init-texmacs-s7.scm` add their exports to `*texmacs-user-module*` itself.
  It ends up holding about a thousand slots in one linked list, and s7 looks
  symbols up in a non-global `let` by linear search when its per-symbol cache
  misses. This is the reason for the local `lookup_from` patch in s7 (see
  [05](05-build-and-history.md#s7-version-and-local-patch)).

### Other definitions

- **`on-entry` and `on-exit`.** `on-entry` evaluates its body immediately.
  `on-exit` chains thunks onto `quit-TeXmacs-scheme`.
- **`has-look-and-feel?`.** Hard-coded to `(== x "emacs")`.
- **`list?` is rebound to `proper-list?`.** In s7, `list?` is true for any
  pair, including dotted and circular ones.
- **`display` and `write`.** With a single argument, they are redirected to
  `tm-output`, so output reaches the TeXmacs console and session widgets.

## 2.3 `tm-define` on s7 (`kernel/texmacs/tm-define.scm`)

The Guile version switched to the `texmacs-user` module, did a
`define-public`, and switched back. The s7 version drops all of that.

- **A first definition installs the value in the rootlet:**
  ```scheme
  (varlet (rootlet) ',var (if (null? cur-conds) ,nval (let ((former …)) ,nval)))
  ```
- **An overload is just `(set! ,var ,nval)`.** It finds the rootlet binding.
- **`tm-defined-module` records `*module-name*`.** Guile used
  `(module-name temp-module)`.
- **`tm-define-macro`** defines the public macro with
  `(with-module *texmacs-user-module* (define-public-macro …))`.
- **`lazy-define`** looks the symbol up with `((resolve-module 'm) 'name)`.
  An s7 `let` applied to a symbol returns its value.
- **Curried property constructors.** `(define ((define-property which) opt decl) …)`
  was rewritten as an explicit lambda. The curried-`define` shim is not
  installed in the rootlet, where this code runs.
- **Procedure names.** Guile has `procedure-name` and s7 does not. The new
  helpers are:
  - `procedure-name` now returns the procedure itself, or `#f`.
  - `procedure-symbol-name` returns the tm-defined symbol if it is known.
    Otherwise it uses the procedure's printed name when that name is
    alphabetic (s7 prints `car` for the builtin and `foo` for
    `(define (foo x) …)`).
  - `procedure-string-name` is the string version.

  `tm-define-test.scm` checks these helpers. The change to `procedure-name`
  breaks `property`; see [06](06-open-issues.md).

`tm-modes.scm` works the same way: generated mode predicates are installed
with `(varlet *texmacs-module* 'pred (lambda () test))`.
`regexp-select.scm` rebinds `select` the same way.
