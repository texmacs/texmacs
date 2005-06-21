;; arch-tag: 3361591e-8833-4688-ac6f-3dd81774176c

(plugin-configure backtrace
  (:require #t)
  (:initialize (debug-enable 'backtrace 'debug)))
