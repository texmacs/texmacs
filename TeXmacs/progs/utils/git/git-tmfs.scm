
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-tmfs.scm
;; DESCRIPTION : tmfs for the Git tools
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils git git-tmfs)
  (:use (version version-git)))

(tm-define (git-show-log)
  (cursor-history-add (cursor-path))
  (revert-buffer "tmfs://git/log"))

(tm-define (git-show-status)
  (cursor-history-add (cursor-path))
  (revert-buffer "tmfs://git/status"))

(tm-define ($staged-file status file)
  (cond ((string-starts? status "A")
         (list 'concat "new file:   " file (list 'new-line)))
        ((string-starts? status "M")
         (list 'concat "modified:   " file (list 'new-line)))
        ((string-starts? status "R")
         (list 'concat "renamed:    " file (list 'new-line)))
        (else "")))

(tm-define ($unstaged-file status file)
  (cond ((string-ends? status "M")
         (list 'concat "modified:   " file (list 'new-line)))
        (else "")))

(tm-define ($untracked-file status file)
  (cond ((== status "??")
         (list 'concat file (list 'new-line)))
        (else "")))

(tm-define (git-status-content)
  (with s (git-status)
        ($generic
         ($when (not s)
                "Not git status available!")
         ($when s
                ($tmfs-title "Git Status")
                ($description-long
                 ($describe-item "Changes to be commited"
                                 ($for (x s)
                                       ($with (status file) x
                                              ($staged-file status
                                                            file))))
                 ($describe-item "Changes not staged for commit"
                                 ($for (x s)
                                       ($with (status file) x
                                              ($unstaged-file status
                                                              file))))
                 ($describe-item "Untracked files"
                                 ($for (x s)
                                       ($with (status file) x
                                              ($untracked-file status
                                                               file)))))))))


(tm-define (git-log-content)
  (with h (git-log)
        ($generic
         ($tmfs-title "Git Log")
         ($when (not h)
                "This directory is not under version control.")
         ($when h
                ($description-long
                 ($for (x h)
                       ($with (date by msg commit) x
                              ($describe-item
                               ($inline "Commit " commit
                                        " by " (utf8->cork by)
                                        " on " date)
                               (utf8->cork msg)))))))))

(tmfs-title-handler (git name doc)
  (cond ((== name "status") "Git Status")
        ((== name "log") "Git Log")
        (else "unknown")))

(tmfs-load-handler (git name)
  (cond ((== name "status")
         (git-status-content))
        ((== name "log")
         (git-log-content))
        (else '())))