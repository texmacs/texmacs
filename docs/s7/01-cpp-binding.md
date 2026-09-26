# 1. The C++ binding

## 1.1 Layering

```
rest of TeXmacs ── scheme.hpp: object, call(), eval(), as_tree() ...   (interpreter-neutral)
                      │
                   Scheme/object.cpp, Scheme/glue.cpp, Glue/glue_*.cpp (generated)
                      │   uses only: tmscm, tmscm_*, *_to_tmscm, tmscm_to_*, TMSCM_*
                      │
      ┌───────────────┼──────────────────┐
  S7/s7_tm.{hpp,cpp}  Guile/guile_tm.*    Tiny/tinyscheme_tm.*
  (USE_S7)            (USE_GUILE)         (not built)
```

`src/Scheme/scheme.hpp` is the API the rest of TeXmacs uses: the `object`
class, `call`, `eval`, `exec_delayed`, and the conversions between objects and
trees. The port added two functions to it:
- `as_list_object`;
- `scheme_init_file ()`, which names the backend's initialization file (§2.1).

`src/Scheme/Scheme/object.hpp` includes `s7_tm.hpp` or `guile_tm.hpp`
according to the build option `USE_S7` / `USE_GUILE` (see
[05](05-build-and-vendored-s7.md)).

## 1.2 `s7_tm.hpp`: the `tmscm` API on s7

- `typedef s7_pointer tmscm;` and a global interpreter `extern s7_scheme *tm_s7;`.
- The predicates, constructors and accessors are thin inline wrappers:
  `tmscm_is_pair` → `s7_is_pair`, `tmscm_cons` → `s7_cons`, and so on.
  - `tmscm_is_double` maps to `s7_is_real`, so it is also true for integers
    and rationals.
  - `tmscm_to_bool` maps to `s7_boolean`.
- **Arity adaptors.** s7 C functions have the signature
  `s7_pointer f(s7_scheme*, s7_pointer args)`, where `args` is a list. The
  templates `proc<PROC>` (0–10 arguments) unpack that list into positional
  arguments for the glue's `tmg_*` functions.
- **`tmscm_install_procedure(name, func, args, p0, p1)`** becomes
  `s7_define_function(tm_s7, name, proc<func>, args, 0, false, "[missing doc]")`.
  The optional and rest counts `p0`/`p1` are ignored.
- **`TMSCM_ASSERT`** becomes `s7_wrong_type_arg_error(tm_s7, subr, pos, arg, "some other thing")`,
  so the "expected type" text in error messages is a placeholder.
- `TMSCM_UNSPECIFIED` is `s7_unspecified(tm_s7)`.

## 1.3 `s7_tm.cpp`

**Startup (`start_scheme`).**

- Calls `s7_init()`.
- Makes the reader read `'x` as `(quote x)`, with
  `(set! (*s7* 'symbol-quote?) #t)`. TeXmacs code inspects quoted forms, as
  in `(== (car x) 'quote)`.
- **Sets the initial heap to 1 M cells**, `(set! (*s7* 'heap-size) 1024000)`.
  That is about what TeXmacs uses once booted, and it saves collections while
  booting and in short tasks.
- Creates `user_env = s7_inlet(tm_s7, nil)`, a fresh empty environment whose
  parent is the rootlet, and GC-protects it.
- Calls TeXmacs's `main` continuation. Unlike Guile's `scm_boot_guile`, s7
  does not need to own the C stack.

**Evaluation.**

- `eval_scheme_file` → `s7_load_with_environment(tm_s7, file, user_env)`.
- `eval_scheme` → `s7_eval_c_string_with_environment(tm_s7, s, user_env)`.
- `call_scheme(fun, a1…)` builds an argument list and calls `s7_call`.
- None of them sets up an error handler at the C level. An uncaught Scheme
  error goes to s7's default handler, which prints it and returns to the top
  level.
- None of them renumbers `user_env`, which matters for lookups (see
  [02](02-boot-and-modules.md#lookup-caching)).

`user_env` becomes `*texmacs-user-module*` (§2.2).

**Backend identity.**

- `scheme_dialect ()` returns `"s7"`; Scheme code sees it as
  `(scheme-dialect)`.
- `scheme_init_file ()` returns `$TEXMACS_PATH/progs/init-s7.scm`.

**Strings and symbols.** `s7_make_string_with_length` and
`s7_string_length` keep lengths, so strings can contain NULs. Symbols go
through `s7_make_symbol` and `s7_symbol_name`.

**Functions defined in C.** Two Guile builtins that s7 lacks:
- `current-time`, via `gettimeofday`;
- `getpid`.

**Initialization (`initialize_scheme`).** Evaluated in the rootlet:
- a small prelude: `display-to-string`, `texmacs-version`, and
  `object-stack`;
- then `initialize_compat`, `initialize_smobs` and `initialize_glue`.

## 1.4 Blackboxes: C++ values inside Scheme

TeXmacs passes C++ values (`tree`, `url`, `widget`, `command`, `observer`,
`modification`, `patch`, …) to Scheme as `blackbox` objects. These are
type-erased, reference-counted boxes.

In s7, a blackbox is an s7 **c-object** of a type registered with
`s7_make_c_type(tm_s7, "blackbox")`. The type has these hooks:

| hook | implementation |
|---|---|
| `gc_free` | `tm_delete` the heap-allocated `blackbox*` |
| `gc_mark` | no-op: a blackbox holds no Scheme references that s7 has to trace (see §1.5) |
| `is_equal` | pointer identity, else C++ `==` on the boxes |
| `to_string` | `<tree …>`, `<url …>`, `<widget>`, `<command>`, … |

`tmscm_is_blackbox` checks `s7_is_c_object` and the tag.
`blackbox_to_tmscm` allocates a `blackbox` with `tm_new` and wraps it.

## 1.5 Keeping Scheme values alive from C++

`object` (in `scheme.hpp`) wraps a `tmscm_object_rep`, and any C++ structure
can hold on to one. The mechanism is the same as with Guile.

- **Roots.** Every rep conses its value onto the Scheme list `object-stack`,
  which is defined in the rootlet and is therefore reachable from GC roots.
- **Release.** Destructors do not touch Scheme, because they may run during
  GC. Instead, they push their handle on `destroy_list`, and the next
  `tmscm_object_rep` constructor unlinks those handles.
- **Consequence for blackboxes.** A C++ object that holds a Scheme closure
  (for example a `command` made by `as_command`) keeps that closure alive
  through `object-stack`, not through the blackbox. That is why
  `mark_blackbox` can be a no-op.

## 1.6 Glue generation

- **Tables.** `src/Scheme/Glue/build-glue-{basic,editor,server}.scm` list
  entries of the form `(scheme-name c_function (ret-type arg-types…))`.
- **Generator.** `build-glue.scm` turns each entry into:
  - a `tmscm tmg_<name>(tmscm…)` wrapper;
  - `TMSCM_ASSERT_<TYPE>` checks and `tmscm_to_<T>` conversions;
  - a call to the C++ function, then `<T>_to_tmscm` on the result;
  - a `tmscm_install_procedure` call in `initialize_glue_*`.
- **The output works with either interpreter,** and so do the generators.
  How to regenerate is in [05](05-build-and-vendored-s7.md#glue-regeneration).
- **Glue added for the port:** `get-user-login` and `get-user-name`. They
  replace Guile's `getpwnam`, `getlogin` and related functions.

## 1.7 Cost of crossing the boundary

Measured with `docs/s7/bench/marshal.scm` (see
[07](07-performance.md#crossing-the-boundary)):

- **A glue call costs about 20 ns** more than a call to an s7 primitive.
- **Strings cost about 0.35 ns per byte** in either direction, i.e. a couple
  of memory copies.
- **Whole trees are the expensive case.** `tree->stree` and `stree->tree`
  take about 400 ns per node. They go through an intermediate C++
  `scheme_tree` whose strings are quoted (`scm_quote`) and then unquoted
  again, and each string is copied about four times.

Even so, marshalling is about 3% of a LaTeX export, and under 1% of
regenerating the manual. A direct tree ↔ s7 conversion, without the
intermediate tree, could make those two functions several times faster.
It hasn't been done because the gain overall would be small.

## 1.8 C++ changes outside `src/Scheme`

- **`glue.cpp` `tmscm_is_content`.** The loop over a list now stops at a
  non-pair and requires a proper list (`tmscm_is_null` at the end). Before, it
  assumed a proper list, which dotted pairs violate.
- **`glue.cpp` uint arguments.** `TMSCM_ASSERT_UINT` and `tmscm_to_uint` are
  interpreter-neutral; the Guile version called `scm_positive_p`.
- **`object.cpp` `eval(object)`.** It calls `tm-eval`, which evaluates in
  `*texmacs-user-module*`, instead of `eval`. In s7, a bare `eval` uses the
  caller's `curlet`.
- **`object.cpp` `operator<<` on a non-console stream.** It uses
  `object->string` instead of printing `FAILED`.
- **`scheme_command_rep::print`.** It prints `(sourcify obj)`.
- **`analyze.cpp` `unescape_guile`** undoes the printer's escapes of control
  characters in `object->tmstring`. s7 writes them `\xHH;` and Guile `\xHH`,
  so the `;` is skipped on s7 builds only.
- **`sys_utils`** (Unix, MinGW, Android): `get_user_login` and
  `get_user_name`, behind the glue above.
- **`tm_server.cpp`** loads `scheme_init_file ()`.
- **Platform files** (Unix, Windows64, Android): the Guile hooks are compiled
  only with `USE_GUILE`.
