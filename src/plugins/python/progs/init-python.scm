(plugin-configure python
  (:require (url-exists-in-path? "tm_python"))
  (:launch "tm_python --texmacs")
  (:tab-completion #t)
  (:session "Python"))
