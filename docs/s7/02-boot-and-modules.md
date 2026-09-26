# 2. Boot sequence and the module system

## 2.1 Boot order

1. **C++.** `start_scheme` and `initialize_scheme` set up s7, the blackbox
   type and the glue (see [01](01-cpp-binding.md)).
2. **The backend's init file.** `tm_server.cpp` loads `scheme_init_file ()`
   into `user_env`. That file is `progs/init-s7.scm` on s7 and
   `progs/init-guile.scm` on Guile.
3. **The dialect file wraps the common ones.** Each dialect file is a plain
   sequence, with no test on the interpreter:
   1. set up the interpreter (see below);
   2. load its module system: `kernel/boot/boot-s7.scm` or `boot.scm` (§2.2);
   3. load its kernel compatibility module: `(kernel boot compat-s7)` or
      `(kernel boot compat)` (see [03](03-compat-layer.md));
   4. load the common `init-kernel.scm`, which imports the kernel;
   5. run its own steps between the common files: on s7,
      `(renumber-user-module!)` (§2.3);
   6. load the common `init-texmacs.scm`, the rest of the initialization
      (`lazy-define`, `lazy-menu`, `lazy-keyboard`, …).

   Code specific to one interpreter goes into its file, before, between or
   after the common ones.

**What `init-s7.scm` sets up.**

- **`symbol?` excludes keywords.** In s7, `:foo` is a symbol and satisfies
  `symbol?`; in Guile it does not. `primitive-symbol?` keeps the original.
- **`*current-module*`** is set to `user_env`, and `tm-eval` evaluates in
  `*texmacs-user-module*`.
- **`load` and `eval`** are wrapped so that they default to
  `*current-module*`, instead of s7's rootlet (for `load`) and `curlet` (for
  `eval`).
- **`catch`** is wrapped so that handlers get Guile's four arguments,
  `(key subr message args)`. `subr` is always `"[not-implemented]"`; the
  other values are taken from s7's `(type info)`.
- **`developer-mode?` is `#f`.** On Guile it reads the `developer tool`
  preference and installs a reader that records where definitions come from.
  Neither exists on s7.

**Macros are s7's native run-time macros.** `define-macro` is s7's own. An
earlier version aliased it to s7's read-time `define-expansion`. With s7 11
that also expanded macros inside quasiquoted templates, which broke the menu
code, and a `load` during an expansion aborted the file being read.

## 2.2 Modules as environments (`boot-s7.scm`)

Guile's module objects are replaced by s7 first-class environments (`let`s).

```
(rootlet) = *texmacs-module*   ← s7 builtins, glue, compat, every tm-define,
   │                             and every public definition (define-public)
   └─ user_env = *texmacs-user-module*   ← top-level definitions, about 240
         └─ one sublet per loaded module  ← the module's own definitions
```

**Globals** (in the rootlet):
- `*texmacs-module*`, the rootlet itself;
- `*module-name*`;
- `*modules*`, a hash table from module names such as `(kernel boot abbrevs)`
  to their environments.

**`texmacs-module name opts…`** expands, in the module's own environment,
to:
- `(define *module-name* 'name)` and `(define *exports* ())`;
- a registration of `(current-module)` in `*modules*`;
- `:use` → `use-modules`, `:inherit` → `inherit-modules`.

`:export` prints a deprecation warning. Any other option is silently ignored.

**`module-load`** resolves the file through `$GUILE_LOAD_PATH` (the name is
historical). It creates a fresh `sublet` of `*texmacs-user-module*` and loads
the file inside `with-module`, which makes the new environment the rootlet's
`*current-module*` while the file loads.

### Public definitions and imports

- **`define-public` and `define-public-macro`** do three things:
  1. an ordinary `define` in the module's environment;
  2. a push of the symbol onto the module's `*exports*`;
  3. a **publication of the binding in the rootlet** (`publish-binding!`).
     That is where every other module finds it.
- **`export`** only pushes onto `*exports*`. Such names are copied by
  `use-modules`.
- **`use-modules m…`**, for each module:
  1. resolves it, loading it if needed;
  2. collects its exported bindings (in s7, `map` over a `let` yields
     `(sym . val)` pairs);
  3. installs them in the current module with `import-bindings!`, which
     handles three cases:
     - **already visible with the same value** (`eq?`) through the target's
       outlets, which is the case for every public definition: nothing is
       done (§2.3 explains why a copy would hurt);
     - **already bound in the target:** `let-set!`, since s7 11 refuses to
       `varlet` a symbol already bound in a non-root let;
     - **otherwise:** `varlet`.
- **`inherit-modules`** is `use-modules` plus re-exporting the inherited
  modules' exports. The exports are collected when the form is evaluated,
  not when it is expanded.
- **`import-from`** is an alias for `use-modules`.

**With `TM_PUBLISH_LOG` set,** publications that replace a different
rootlet value are reported. Today there are four:
- `assoc-set!`, defined twice in `compat-s7.scm`;
- `list-tail`, `string->keyword` and `help`, which TeXmacs redefines with
  meanings compatible with s7's builtins.

### Consequences

- **Every other module shares one binding per public name, but the defining
  module keeps its own.** If the defining module later does a `set!` of a
  public variable, only its own binding changes. Guile modules share the
  variable instead. This matters little in practice: definitions that are
  meant to be overridden use `tm-define`, which also lives in the rootlet.
- **Modules see everything.** Every module sees the rootlet and the user
  module, not only what it declared with `:use`, so missing `:use` clauses
  go unnoticed. Guile's TeXmacs modules also use `texmacs-user`, so the
  visibility is the same.

<a id="lookup-caching"></a>
## 2.3 How lookups stay fast

**How s7 finds a name.**

- For each symbol, s7 caches its most recent *local* binding (outside the
  rootlet) and the id of the let that holds it.
- Lets get increasing ids when they are created, and entering a let with
  `with-let` (and so `with-module`) gives it a fresh, highest id.
- A lookup walks outward from the current let:
  - it skips the lets that are newer than the cached binding;
  - it answers in O(1) when it reaches the let of the cached binding;
  - it scans the slots of every other older let, one by one;
  - it ends in the rootlet, where bindings are found directly.

So a lookup is cheap when the lets on the way are newer than the name's
cached binding, and costly when it has to scan large lets.

**What that means for TeXmacs.** A public name like `==` is bound in its
module (created after the user module) and in the rootlet. From another
module, a lookup skips the frames and the module's let, which are newer. It
must not stop in the user module, and three rules make sure of that:

- **`renumber-user-module!`** (`boot-s7.scm`) enters the user module once,
  right after the kernel is loaded (in `init-s7.scm`). The user module then
  becomes newer than the kernel modules, so lookups of kernel names skip it
  and end in the rootlet. Without this step, repeated LaTeX export is more
  than twice as slow.
- **Nothing enters the user module afterwards.** It would then become newer
  than every module loaded so far. Lookups from their code of the names that
  live in the user module itself would then scan their environment. These
  are mainly `define`, the curried `define` of `compat-s7.scm`.
  - `tm-define-macro` therefore defines its macro with `eval` in the user
    module, which does not renumber it (§2.5).
  - The C++ entry points don't renumber either (§1.3).
- **`import-bindings!` does not copy a binding that a module already sees
  with the same value.** The copy would move the symbol's cached binding
  into the importing module, and lookups from everywhere else would scan
  again.

**Two names bound outside the rootlet on purpose:**
- `list?`, rebound to `proper-list?`, is bound in the rootlet like the
  public definitions;
- the curried `define` stays in the user module, because rebinding `define`
  itself in the rootlet silently ends s7's current `load`.

With these rules TeXmacs runs on s7 as released. An earlier version copied
every export into the user module, which grew to about a thousand bindings.
It needed a patch in s7's lookup, and even then repeated LaTeX export was
slower than on Guile.

## 2.4 Other definitions in `boot-s7.scm`

- **`on-entry` and `on-exit`.** `on-entry` evaluates its body immediately;
  `on-exit` chains thunks onto `quit-TeXmacs-scheme`.
- **`has-look-and-feel?`** is hard-coded to `(== x "emacs")`.
- **`list?` is `proper-list?`.** In s7, `list?` is true for any pair,
  including dotted and circular ones.
- **`display` and `write`** with a single argument are redirected to
  `tm-output`, so output reaches the TeXmacs console and session widgets.

## 2.5 `tm-define` on s7 (`kernel/texmacs/tm-define.scm`)

The Guile version switches to the `texmacs-user` module, does a
`define-public`, and switches back. The s7 version works on the rootlet
directly.

- **A first definition installs the value in the rootlet:**
  ```scheme
  (varlet (rootlet) ',var (if (null? cur-conds) ,nval (let ((former …)) ,nval)))
  ```
- **An overload is just `(set! ,var ,nval)`,** which finds the rootlet
  binding.
- **`tm-defined-module` records `*module-name*`.**
- **`tm-define-macro`** defines the public macro with
  `(eval '(define-public-macro …) *texmacs-user-module*)` (see §2.3).
- **`lazy-define`** looks the symbol up with `((resolve-module 'm) 'name)`:
  an s7 `let` applied to a symbol returns its value.
- **Procedure names.** s7 has no `procedure-name`. The helpers are:
  - `procedure-name` returns the procedure itself, or `#f`;
  - `procedure-symbol-name` returns the tm-defined symbol if it is known.
    Otherwise it uses the procedure's printed name, unless that starts with
    `#<`, which is how s7 prints anonymous procedures;
  - `procedure-string-name` is the string version.

  `property`, `lazy-define-force` and the interactive-command helpers use
  `procedure-symbol-name`.

The same approach is used elsewhere:
- `tm-modes.scm` installs mode predicates with
  `(varlet *texmacs-module* 'pred (lambda () test))`, and registers their
  names;
- `regexp-select.scm` rebinds `select` in the rootlet.
