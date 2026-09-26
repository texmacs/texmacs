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
  (active)            (not compiled)      (not compiled)
```

`src/Scheme/scheme.hpp` is the API the rest of TeXmacs uses: the `object`
class, `call`, `eval`, `exec_delayed`, and the conversions between objects and
trees. Its only s7-era change is the new `as_list_object`.

You pick a backend with one include in `src/Scheme/Scheme/object.hpp`:

```cpp
//#include "../Tiny/tinytmscm_tm.hpp" // interface to TinyScheme
//#include "../Guile/guile_tm.hpp" // interface to guile
#include "../S7/s7_tm.hpp" // interface to S7
```

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
  The optional and rest counts `p0`/`p1` are ignored (there is a FIXME). No
  glue function has documentation.
- **`TMSCM_ASSERT`** becomes `s7_wrong_type_arg_error(tm_s7, subr, pos, arg, "some other thing")`,
  so the "expected type" text in error messages is a placeholder.
- `TMSCM_UNSPECIFIED` is `s7_unspecified(tm_s7)`.

## 1.3 `s7_tm.cpp`

### Startup (`start_scheme`)

- Calls `s7_init()`.
- Creates `user_env = s7_inlet(tm_s7, nil)`, a fresh empty environment whose
  parent is the rootlet, and GC-protects it.
- Calls TeXmacs' `main` continuation. Unlike Guile's `scm_boot_guile`, s7
  does not need to own the C stack.

### Evaluation

- `eval_scheme_file` → `s7_load_with_environment(tm_s7, file, user_env)`.
- `eval_scheme` → `s7_eval_c_string_with_environment(tm_s7, s, user_env)`.
- Neither sets up an error handler at the C level. An uncaught Scheme error
  goes to s7's default handler, which prints it and returns to the top level.

**Note: the C++ entry points evaluate in `user_env`.** The Scheme boot (see
§2) defines its own `*texmacs-user-module*` as the `curlet` at the point where
`boot-s7.scm` is loaded. `init-texmacs.scm` is loaded through
`eval_scheme_file`, and it loads `init-s7.scm` into its own environment, so
that is `user_env`.

### Calls

`call_scheme(fun, a1…)` builds an argument list and calls `s7_call`.

### Strings and symbols

- `s7_make_string_with_length` and `s7_string_length`: strings keep their
  length and can contain NULs.
- Symbols go through `s7_make_symbol` and `s7_symbol_name`.

### `scheme_dialect()`

Returns `"s7"`. Scheme code sees this as `(scheme-dialect)`.

### Compatibility functions added in C

These Guile builtins are missing from s7, so they are defined in C:

- `current-time`, via `gettimeofday`.
- `getpid`. A FIXME suggests `QCoreApplication::applicationPid()` for
  portability.

### Initialization (`initialize_scheme`)

Evaluated in the rootlet:

- A small prelude: `display-to-string`, `texmacs-version`, and
  `object-stack` (`'(())`).
- Then `initialize_compat`, `initialize_smobs` and `initialize_glue`.

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
can hold on to one.

- **Roots.** Every rep conses its value onto the Scheme list `object-stack`,
  which is defined in the rootlet and is therefore reachable from GC roots.
- **Release.** Destructors do not touch Scheme, because they may run during
  GC. Instead, they push their handle on `destroy_list`, and the next
  `tmscm_object_rep` constructor unlinks those handles.
- **Consequence for blackboxes.** A C++ object that holds a Scheme closure
  (for example a `command` made by `as_command`) keeps that closure alive
  through `object-stack`, not through the blackbox. That is why
  `mark_blackbox` can be a no-op.

This mechanism is the same one the Guile backend used. The s7 port did not
change it.

## 1.6 Glue generation

- **Tables.** `src/Scheme/Glue/build-glue-{basic,editor,server}.scm` list
  entries of the form `(scheme-name c_function (ret-type arg-types…))`.
- **Generator.** `build-glue.scm` turns each entry into:
  - a `tmscm tmg_<name>(tmscm…)` wrapper;
  - `TMSCM_ASSERT_<TYPE>` checks;
  - `tmscm_to_<T>` conversions;
  - a call to the C++ function, then `<T>_to_tmscm` on the result;
  - a `tmscm_install_procedure` call in `initialize_glue_*`.
- **The output works with either interpreter.** Only the
  `tmscm_install_procedure` macro changes per backend.
- **Regenerating still needs a `guile` binary.** The `build-glue` shell script
  runs `guile -l build-glue.scm …`. The generated `glue_*.cpp` files are
  committed.
- **Glue added on this branch:**
  - s7-related: `get-user-login` and `get-user-name`. They replace Guile's
    `getpwnam`, `getlogin` and related functions.
  - Plumbing: `command-eval` and `command-apply`.
  - Unrelated upstream additions: PDF attachments, `os-android?`, and others.

## 1.7 Small C++ changes made for s7

- **`glue.cpp` `tmscm_is_content`.** The loop over a list now stops at a
  non-pair and requires a proper list (`tmscm_is_null` at the end). Before, it
  assumed a proper list, which dotted pairs violate.
- **`object.cpp` `eval(object)`.** It now calls `tm-eval`, which evaluates in
  `*texmacs-user-module*`, instead of `eval`. In s7, a bare `eval` uses the
  caller's `curlet`.
- **`object.cpp` `operator<<` on a non-console stream.** It now uses
  `object->string` instead of `FAILED`.
- **`scheme_command_rep::print`.** It prints `(sourcify obj)`.
- **`analyze.cpp` `unescape_guile`.** It understands s7's `\xHH;` string
  escapes (commit `29481b3441`).
