(texmacs-module (utils git git-tmfs)
  (:use (utils git git-utils)))

(tm-define (git-show-log)
  (cursor-history-add (cursor-path))
  (revert-buffer "tmfs://git/log"))

(tm-define (git-show-status)
  (cursor-history-add (cursor-path))
  (revert-buffer "tmfs://git/status"))

(tm-define (git-history name)
  (cursor-history-add (cursor-path))
  (with s (url->tmfs-string name)
        (revert-buffer (tmfs-url-git_history s))))

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

(tm-define (tmfs-url-git_history . content)
  (string-append "tmfs://git_history/"
                 (string-concatenate content)))

(tmfs-title-handler (git_history name doc)
  (with u (tmfs-string->url name)
        (string-append (url->system (url-tail u)) " - History")))

(tmfs-load-handler (git_history name)
  (with u (tmfs-string->url name)
        (with h (buffer-log u)
              ($generic
               ($tmfs-title "History of "
                            ($link (url->unix u)
                                   ($verbatim (utf8->cork (url->system (url-tail u))))))
               ($when (not h)
                      "This file is not under version control.")
               ($when h
                      ($description-long
                       ($for (x h)
                             ($with (date by msg commit) x
                                    ($describe-item
                                     ($inline "Commit " commit
                                              " by " (utf8->cork by)
                                              " on " date)
                                     (utf8->cork msg))))))))))

(tm-define (tmfs-url-commit . content)
  (string-append "tmfs://commit/"
                 (string-concatenate content)))
(tmfs-format-handler (commit name)
  (if (string-contains name "|")
      (with u (tmfs-string->url (tmfs-cdr (string-replace name "|" "/")))
            (url-format u))
      (url-format (tmfs-string->url name))))

(tm-define (git-show-normal name)
  (define (sum2 x)
    (+ (first x) (second x)))
  (define (length-of-2col x)
    (+ (string-length (number->string (sum2 x)))
       (fourth x)))
  
  (let* ((m (git-commit-message name))
         (p (git-commit-parent name))
         (d (git-commit-diff p name))
         (nr (length d))
         (ins (list-fold + 0 (map first d)))
         (del (list-fold + 0 (map second d)))
         (maxv (list-fold max 0 (map sum2 d)))
         (maxs (- 81 (list-fold max 0 (map length-of-2col d)))))
    ($generic
         ($tmfs-title "Commit Message of " (string-take name 7))
         (if (== name p)
             "parent 0"
             `(concat "parent "
                      ,($link (tmfs-url-commit p) p)))
         (list 'new-line)
         ($for (x m) `(concat ,(utf8->cork x) ,(list 'new-line)))
         "-----"
         (list 'new-line)
         `(verbatim
           (tabular
            (tformat
             (cwith "1" "-1" "1" "-1"
                    cell-lsep "0pt")
             ,(cons 'table
                    (map (lambda (x) (get-row-from-x x maxs maxv)) d)))))
         (list 'new-line)
         `(concat ,nr " files changed, "
                  ,ins
                  " insertions(" (verbatim (with color green "+")) "), "
                  ,del
                  " deletions(" (verbatim (with color red "-")) ")"))))

(tm-define (git-show-merge name)
  (let* ((parents (git-commit-parents name))
         (left (car parents))
         (right (car (cdr parents))))
    ($generic ($tmfs-title "Merge")
            `(concat "parents "
                     ,($link (tmfs-url-commit left) left)
                     ,(list 'new-line)
                     ,($link (tmfs-url-commit right) right)))))

(tmfs-load-handler (commit name)
  (if (string-contains name "|")
      (git-show (string-replace name "|" ":"))
      (if (== (length (git-commit-parents name)) 1)
          (git-show-normal name)
          (git-show-merge name))))

(tm-define (string->commit str name)
  (if (string-null? str) '()
      (with alist (string-split str #\nl)
            (list (string-take (first alist) 20)
                  (second alist)
                  (third alist)
                  ($link (tmfs-url-commit (fourth alist)
                                          (if (string-null? name)
                                              ""
                                              (string-append "|" name)))
                         (string-take (fourth alist) 7))))))

(define (string-repeat str n)
  (do ((i 1 (1+ i))
       (ret "" (string-append ret str)))
      ((> i n) ret)))

(define (get-row-from-x x maxs maxv)
  (define (get-length nr)
    (let* ((ret (if (== maxv 0)
                    0
                    (/ (* nr (min maxs maxv)) maxv))))
      (if (and (> ret 0) (< ret 1))
          1
          ret)))
  `(row (cell ,(third x))
        (cell ,(number->string (+ (first x) (second x))))
        (cell (concat (with color green
                            ,(string-repeat "+"
                                            (get-length (first x))))
                      (with color red
                            ,(string-repeat "-"
                                            (get-length (second x))))))))
