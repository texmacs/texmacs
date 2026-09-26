# 6. Open issues, fragile spots and next steps

## 6.1 Open bugs and limitations

- **The s7 optimizer can mis-apply closures called from a loop.** This is
  an upstream bug, present in s7 11.9 and worked around in TeXmacs. After a
  loop has run once with a closure of one shape, calling it with a closure
  of another shape can run the wrong one:

  ```scheme
  (define (mk s) (let ((chars (string->list s))) (lambda (ch) (and (memv ch chars) #t))))
  (define (inter . css) (lambda (ch) (let loop ((cl css)) (or (null? cl) (and ((car cl) ch) (loop (cdr cl)))))))
  (define (count cs) (let loop ((i 0) (n 0)) (if (= i 256) n (loop (+ i 1) (if (cs (integer->char i)) (+ n 1) n)))))
  (count (inter (mk "!?") (mk "aB!")))   ; => 1
  (count (mk "abc"))                     ; => error: memv second argument, #\null, ... should be a list
  ```

  - **Workaround:** the char-sets of `compat-s7.scm` are hash tables, not
    closures, which is how the bug first showed up. Other higher-order code
    could hit it too, for example `string-index` with a predicate.
  - **To do:** report it upstream with this reproduction.
- **Macros can lose their internal definitions.** This is also upstream, and
  worked around. A macro whose body defines helper functions can lose them
  between recursive calls of those helpers.
  - **Reproduction:** load the original `case-lambda` of `srfi.scm` in a let
    whose `define` is `curried-define`, then evaluate
    `(let ((f (case-lambda ((x) 1) ((x y) 2)))) (f 1))`. It raises
    `unbound variable alength`.
  - **Workaround:** TeXmacs macros define their helpers at module level
    (§3.2).
  - **To do:** report it upstream.
- **Memo tables no longer cache `#f`.** Storing `#f` in an s7 hash table
  doesn't create an entry, so `logic-holds?` (`logic-data.scm`) and
  `texmacs-submode?` (`tm-modes.scm`) recompute negative answers on every
  call. Results stay correct, but the work is repeated. **Fix:** store a
  sentinel value.
- **The apidoc source scanner uses Guile-only functions (not verified).**
  `doc/apidoc-funcs.scm` (`parse-form`) uses `source-property` and
  `def-keywords`, which exist only on Guile. The "module exported symbols"
  part of the API docs probably fails on s7.
- **`texmacs-module` ignores unknown options silently.** A misplaced
  parenthesis in a module header therefore goes unnoticed; it once kept
  `cite-sort-test` from loading.
- **`run-all-tests` stops at the first failing suite,** which hides the
  later ones. `docs/s7/bench/suites.scm` runs the portable suites one by one.

## 6.2 Fragile or surprising behavior

- **The user module must not be entered** with `with-let` or `with-module`
  after boot. It would make lookups slower, not wrong (§2.3).
- **A `set!` of a public variable in its own module** is not seen by the
  other modules, which use the rootlet binding (§2.2).
- **`:use` is not enforced.** Every module sees the rootlet and the user
  module.
- **Macros are expanded on every evaluation** (s7 run-time macros).
- **s7 11 quirks to keep in mind when writing kernel code:**
  - `varlet` refuses already-bound symbols in non-root lets;
  - macro bodies should not define local helper functions (§3.2);
  - shared files can't use s7 reader syntax such as `#_define`.
- **`curried-define` exists only in `*texmacs-user-module*`.** Code evaluated
  in the rootlet can't use `(define ((f a) b) …)`.
- **Some compat functions differ from their Guile originals:**
  - `assoc-set!` returns a new list instead of mutating, and it is defined
    twice in `compat-s7.scm`;
  - `ahash-get-handle` returns a fresh cons, so a `set-cdr!` on it doesn't
    write through (no caller does this today);
  - `string-index` and `string-rindex` take no start/end arguments;
  - `iota` takes only one argument;
  - `append!` and `delq` are non-destructive;
  - `lazy-catch` unwinds before running the handler.
- **`with-global` is not unwind-safe:** a non-local exit leaves the variable
  changed. The Guile version has the same problem.
- **Glue error messages are vague.** Argument errors say
  `"some other thing"` instead of the expected type, and
  `tmscm_install_procedure` ignores the optional and rest argument counts.
- **`developer-mode?` is `#f` on s7.** The Guile version reads the
  preference.
- **s7 needs more memory than Guile** on large workloads: about 90 MB more
  for repeated LaTeX export, and 80 MB more for regenerating the manual
  (see [07](07-performance.md#memory)).
- **Output differs slightly between the interpreters:**
  - LaTeX: the order of packages, `---` versus `\textemdash`;
  - HTML: the order of attributes, float digits.

  Reference outputs made with one interpreter won't match the other.

## 6.3 Guile leftovers

- **Guile-flavoured names:**
  - the function `init_guile` in `init_texmacs.cpp`;
  - the empty `texmacs_init_guile_hooks` in s7 builds;
  - `$GUILE_LOAD_PATH`, still the module search path;
  - `unescape_guile`.
- **Scheme files that work only under Guile:**
  - `init-guile.scm`, `kernel/boot/boot.scm` and `kernel/boot/compat.scm`,
    which only the Guile boot loads;
  - `utils/misc/doxygen.scm`, which uses `(ice-9 rdelim)`;
  - the trace facility in `kernel/boot/debug.scm`, which uses
    `procedure-property`.

## 6.4 Before merging upstream

1. **Build and test on Linux and Windows** (MinGW and MSVC). Only macOS has
   been built and run; the Linux, Windows and Android build files were
   edited but not built.
2. **Exercise plugins and user code.** Scheme code written for Guile, such
   as `my-init-texmacs.scm` or plugin `progs`, can use features that
   `compat-s7.scm` lacks or only partly provides (§6.2). No plugin has been
   tried yet.
3. **Split the branch into a reviewable series:**
   - the build option, the vendored s7 and the C++ binding;
   - the shared kernel with its dialect branches;
   - the compatibility layer and the s7 boot;
   - the fixes and speed-ups to shared code, which are useful even with
     Guile (§4.3).

   Upstream should regenerate `configure` itself.
4. **Report the two s7 bugs above upstream.**
5. **Look at the Qt part of the boot.** It dominates boot time on both
   interpreters (see [07](07-performance.md#boot)).
6. **Optional:**
   - a direct tree ↔ s7 conversion for `tree->stree` and `stree->tree`
     (§1.7);
   - tuning of the s7 heap if memory matters more than speed (see
     [07](07-performance.md#memory)).
