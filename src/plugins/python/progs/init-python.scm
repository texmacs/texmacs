(plugin-configure python
  (:require (url-exists-in-path? "python"))
  (:launch "tm_python --texmacs")
  (:tab-completion #t)
  (:session "Python"))
