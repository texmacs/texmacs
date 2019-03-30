(texmacs-module (utils git git-menu)
  (:use (utils git git-utils)
        (utils git git-tmfs)))

(menu-bind git-menu
  ("Log" (git-show-log))
  ("Status" (git-show-status))
  ("Commit" (git-interactive-commit))
  ---
  (when (buffer-to-add? (current-buffer))
            ("Add" (git-add (current-buffer))))
  (when (buffer-to-unadd? (current-buffer))
            ("Undo Add" (git-unadd (current-buffer))))
  (when (buffer-histed? (current-buffer))
        ("History" (git-history (current-buffer))))
  (=> "Compare"
      (when (buffer-tmfs? (current-buffer))
                ("With current version"
                  (git-compare-with-current (current-buffer))))
      (when (buffer-tmfs? (current-buffer))
                ("With parent version"
                  (git-compare-with-parent (current-buffer))))
      (when (and (not (buffer-tmfs? (current-buffer)))
                 (buffer-has-diff? (current-buffer)))
            ("With the master"
              (git-compare-with-master (current-buffer))))))

(menu-bind texmacs-extra-menu
  (former)
  (if (git-versioned? (current-buffer))
      (=> "Git"
          (link git-menu))))
