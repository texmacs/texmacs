(define (python-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u))
      (string-append  s  "\n<EOF>\n"))))

(plugin-configure graphs
  (:launch "tm_graphs --texmacs")
  (:serializer ,python-serialize)
  (:session "Graph"))
