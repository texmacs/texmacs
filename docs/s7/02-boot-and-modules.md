# 2. Boot sequence and the module system

## 2.1 Boot order

1. **C++ prelude.** `start_scheme` → `initialize_scheme` evaluates a small
   prelude in the rootlet and registers the blackbox type and the glue
   (see [01](01-cpp-binding.md)).
2. **Init file.** `tm_server_rep::tm_server_rep` loads the initialization
   file of the Scheme backend, `scheme_init_file ()`, into `user_env`
   (`src/Texmacs/Server/tm_server.cpp`). It is `progs/init-s7.scm` on s7
   (`s7_tm.cpp`) and `progs/init-guile.scm` on Guile (`guile_tm.cpp`).
3. **The dialect file wraps the common ones.** Each dialect file is a plain
   sequence, with no test on the interpreter:
   1. set up the interpreter;
   2. load its module system (`boot-s7.scm` or `boot.scm`);
   3. load its kernel compatibility module (`compat-s7` or `compat`);
   4. load the common `init-kernel.scm`, which imports the kernel;
   5. run its own steps between the two common files, which on s7 is
      `(renumber-user-module!)`;
   6. load the common `init-texmacs.scm`, the rest of the initialization.

   Code specific to one interpreter therefore goes into its file, before,
   between or after the common ones.

   `init-s7.scm` installs these rebindings:
   `init-s7.scm` installs these rebindings:
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
4. **Module system.** `kernel/boot/boot-s7.scm` is loaded (see §2.2), then
   `(kernel boot compat-s7)` ([03](03-compat-layer.md)), then the kernel
   modules with `inherit-modules` (`init-kernel.scm`). Right after the kernel,
   `(renumber-user-module!)` is called, once (see
   [Lookup caching](#lookup-caching)).
5. **The rest of the init file** is common to both interpreters
   (`lazy-define`, `lazy-menu`, `lazy-keyboard`, …). On s7:
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

- **`define-public`, `define-public-macro`.** A normal `define` in the
  module's environment, a push of the symbol onto the module's `*exports*`,
  and a **publication of the binding in the rootlet** (`publish-binding!`),
  which is where the other modules find it (since 2026-09-26).
  - With `TM_PUBLISH_LOG` set, publications that replace a different
    rootlet value are reported. Today there are four:
    - `assoc-set!`, which is defined twice;
    - `list-tail`, `string->keyword` and `help`, s7 builtins that TeXmacs
      redefines with compatible meanings.
- **`export`** only pushes onto `*exports*`. Such names are copied by
  `use-modules` as before.
- **`use-modules m…`.** For each module:
  1. Resolve it, loading it if needed.
  2. Walk the module's environment. In s7, `map` over a `let` yields
     `(sym . val)` pairs.
  3. Keep the pairs whose symbol is in `*exports*`.
  4. Install them in the rootlet's `*current-module*`, which is whichever
     module is loading at that moment. `import-bindings!` does this, in one
     of three ways:
     - **Already bound in the target:** it updates the existing binding with
       `let-set!`. s7 11 refuses to `varlet` a symbol that is already bound
       in the target let ("duplicate identifier"). s7 10 used to add a
       shadowing slot instead.
     - **Already visible with the same value** (`eq?`) through the target's
       outlets, typically a public definition published in the rootlet: it
       does nothing. The copy would not change what lookups return, but it
       would move the symbol's lookup cache into the target (see
       [Lookup caching](#lookup-caching)).
     - **Otherwise:** it `varlet`s the new symbol.
- **`inherit-modules`.** `use-modules` plus re-exporting the imported
  modules' exports. `re-export-modules!` collects those exports when the form
  is evaluated, not when it is expanded. The old version resolved, and so
  loaded, the modules at expansion time. With read-time macros on s7 11, a
  `load` during an expansion silently ended the load of the file being read,
  and the rest of the init file just vanished.
- **`import-from`.** An alias for `use-modules`.

### Consequences

- **Public names have one shared binding, but the defining module has its
  own.** The other modules all see the rootlet binding. If the defining
  module later does a `set!` of a `define-public` variable, only its own
  binding changes. Guile modules share the variable instead.
  - This matters little in practice, because the definitions most often
    overridden are made with `tm-define`, and those live in the rootlet
    too (§2.3).
  - Names exported with a plain `export` are still copied into importers.
- **Modules see all of the user environment.** Every module's parent is
  `*texmacs-user-module*`, so every module can see everything imported at top
  level, not just what it declared with `:use`. Missing `:use` clauses are
  therefore not detected.
- **The user module is small.** Before 2026-09-26, `use-modules` at top
  level copied every export into `*texmacs-user-module*`. It held about a
  thousand slots in one linked list, which s7 scans when its per-symbol
  cache misses, and TeXmacs needed a patch in s7's lookup to be fast. With
  the exports in the rootlet, it holds about 240 slots, and TeXmacs runs on
  s7 as released (see
  [05](05-build-and-history.md#s7-version-and-local-patch)).

<a id="lookup-caching"></a>
### Lookup caching and the user module

**How s7 finds a name.**

- For each symbol, s7 caches its most recent *local* binding (outside the
  rootlet) and the id of the let that holds it.
- Lets get increasing ids when they are created.
- A lookup walks outward from the current let:
  - it skips the lets that are newer than the cached binding;
  - it answers in O(1) if it reaches the let of the cached binding;
  - it otherwise scans the slots of each older let;
  - it ends in the rootlet, whose bindings are found directly.
- Entering a let with `with-let` (and so `with-module`) gives it a fresh,
  highest id.

**What that means for modules.**

- A public name is bound in its module, which was created after the user
  module, and in the rootlet.
- A lookup from another module skips the frames and the (newer) module let,
  and reaches the user module.
- For kernel names this should cost nothing, and three rules make sure of
  it:
  - **`renumber-user-module!`** (`boot-s7.scm`) enters the user module once,
    right after the kernel is loaded (`init-s7.scm`). The user module then
    becomes newer than the kernel modules that bind the kernel names, so
    lookups of kernel names skip it too and end in the rootlet. Without this
    step, 8 LaTeX exports take 6.4 s instead of 2.8 s.
  - **Nothing enters the user module afterwards.** It would become newer
    than every module loaded so far. Lookups from their code of the few
    names bound in the user module itself (mainly `define`, the curried
    `define` of `compat-s7.scm`) would then scan their environment.
    - `tm-define-macro` therefore uses `eval`, which sets the current let
      without renumbering it (§2.3).
    - `eval_scheme` and `call_scheme` from C++ use `s7_eval` and `s7_call`,
      which don't renumber either.
  - **`import-bindings!`** does not copy a binding that a module already
    sees with the same value (above). The copy would move the symbol's cache
    into the importer.

**`define` can't move to the rootlet.** Rebinding `define` itself in the
rootlet silently ends s7's current `load`. So the curried-`define` shim
stays in the user module.

**History.** Before the exports were published in the rootlet, the kernel
names lived in the user module. Several things had to be fixed:

- `tm-define-macro` entered the user module 211 times during a LaTeX export,
  which made the export about three times slower (see
  [07](07-benchmark.md#why-the-warm-latex-export-was-slow));
- a patch in s7's lookup was needed (§5.2);
- the placement of the renumbering was a trade-off.

### Other definitions

- **`on-entry` and `on-exit`.** `on-entry` evaluates its body immediately.
  `on-exit` chains thunks onto `quit-TeXmacs-scheme`.
- **`has-look-and-feel?`.** Hard-coded to `(== x "emacs")`.
- **`list?` is rebound to `proper-list?`, in the rootlet.** In s7, `list?`
  is true for any pair, including dotted and circular ones.
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
  `(eval '(define-public-macro …) *texmacs-user-module*)`. Until 2026-09-25 it
  used `with-module`, which renumbered the user module each time (see
  [Lookup caching](#lookup-caching)).
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
