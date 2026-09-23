
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-tmfs.scm
;; DESCRIPTION : support for external versioning tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;               (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-tmfs))

(define version-tool-table (make-ahash-table))
(define version-tool-loaded (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch to support for various external tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (svn-active? name)
  (let* ((dir (url-head name))
         (anc (url-append dir (url-ancestor)))
         (svn (url-append anc ".svn"))
         (l   (cDr (url->list (url-expand svn)))))
    (list-or (map url-directory? l))))

(tm-define (git-active? name)
  (let* ((dir (if (url-directory? name) name (url-head name)))
         (anc (url-append dir (url-ancestor)))
         (git (url-append anc ".git"))
         (l   (cDr (url->list (url-expand git)))))
    (list-or (map url-directory? l))))

(tm-define (version-tool name)
  (or (if (ahash-ref version-tool-table name)
          (with tool (ahash-ref version-tool-table name)
            (and (!= tool "") tool))
          (with tool
              (cond ((svn-active? name) "svn")
                    ((git-active? name) "git")
                    (else ""))
            (ahash-set! version-tool-table name tool)
            (when (and tool (not (ahash-ref version-tool-loaded tool)))
              (ahash-set! version-tool-loaded tool #t)
              (cond ((== tool "svn")
                     (module-provide '(version version-svn)))
                    ((== tool "git")
                     (module-provide '(version version-git)))))
            (and (!= tool "") tool)))
      (and-with base (url-wrap name)
        (and (version-tool base) "wrap"))))

(tm-define (version-tool* name)
  (if (version-revision? name)
      (version-tool* (version-head name))
      (version-tool name)))

(tm-define (versioned? url)
  (or (nnot (version-tool url))
      (and-with base (url-wrap url)
        (versioned? base))))

(tm-define (version-status name)
  ;; Getting the main status of a file:
  ;; + unknown
  ;; + modified
  ;; + unmodified
  (if (version-tool name)
      (version-status name)
      "unknown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-supports-history? name) (versioned? name))

(tm-define (version-history name) #f)

(tm-define (version-history* name)
  (if (version-revision? name)
      (version-history* (version-head name))
      (version-history name)))

(tm-define (version-newer? name1 name2)
  (let* ((rev1 (version-get-revision* name1))
         (rev2 (version-get-revision* name2))
         (l (cons "" (map car (version-history* name2))))
         (i1 (list-find-index l (cut == <> rev1)))
         (i2 (list-find-index l (cut == <> rev2))))
    (and i1 i2 (< i1 i2))))

(tm-define (version-show-history url)
  ;; Show the history of the URL:
  ;;   1. add the cursor path to the history
  ;;   2. revert the buffer to tmfs
  ;;   3. set the master of tmfs to the URL
  (cursor-history-add (cursor-path))
  (with s (url->tmfs-string url)
    (revert-buffer-revert (string-append "tmfs://history/" s))
    (buffer-set-master (current-buffer) url)))

(tmfs-title-handler (history name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - History")))

(tm-define (version-revision-url u rev)
  (if (string-contains? rev ":")
    (string-append "tmfs://revision/" (string-replace rev ":" "/"))
    (string-append "tmfs://revision/" rev "/" (url->tmfs-string u))))

(tmfs-load-handler (history name)
  (let* ((u (tmfs-string->url name))
         (h (version-history u))
         (Version (if (git-active? u) "Commit" "Version")))
    ($generic
     ($tmfs-title "History of "
                  ($link (url->system u)
                    ($verbatim (utf8->cork (url->system (url-tail u))))))
     ($when (not h)
       "This file is not under version control.")
     ($when h
       ($description-long
         ($for (x h)
           ($with (rev by date msg) x
             ($with rev* (version-beautify-revision u rev)
               ($with dest (version-revision-url u rev)
                 ($describe-item
                     ($inline Version " " ($link dest rev*)
                              " by " (utf8->cork by) " on " date)
                   (utf8->cork msg)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing a particular commit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-url-commit root rev)
  (string-append "tmfs://commit/" rev "/" (url->tmfs-string root)))

(tmfs-format-handler (commit name)
  (url-format (tmfs-string->url (tmfs-cdr name))))

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

(tm-define (git-show-normal root rev)
  (define (sum2 x)
    (+ (first x) (second x)))
  (define (length-of-2col x)
    (+ (string-length (number->string (sum2 x)))
       (fourth x)))
  
  (let* ((m (git-commit-message root rev))
         (p (git-commit-parent root rev))
         (d (git-commit-diff root p rev))
         (nr (length d))
         (ins (list-fold + 0 (map first d)))
         (del (list-fold + 0 (map second d)))
         (maxv (list-fold max 0 (map sum2 d)))
         (maxs (- 81 (list-fold max 0 (map length-of-2col d)))))
    ($generic
         ($tmfs-title "Commit Message of " (version-beautify-revision root rev))
         (if (== rev p)
             "parent 0"
             `(concat "parent "
                      ,($link (tmfs-url-commit root p) p)))
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

(tm-define (git-show-merge root rev)
  (let* ((parents (git-commit-parents root rev))
         (left (car parents))
         (right (car (cdr parents))))
    ($generic ($tmfs-title "Merge")
            `(concat "parents "
                     ,($link (tmfs-url-commit root left) left)
                     ,(list 'new-line)
                     ,($link (tmfs-url-commit root right) right)))))

(tmfs-load-handler (commit name)
  (let* ((root (tmfs-string->url (tmfs-cdr name)))
         (tool (version-tool root)) ;; NOTE: forces lazy loading
         (rev (tmfs-car name)))
    (if (== (length (git-commit-parents root rev)) 1)
        (git-show-normal root rev)
        (git-show-merge root rev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing a particular revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-revision? u)
  (and (url-rooted-tmfs? u)
       (with (class name) (tmfs-decompose-name u)
         (== class "revision"))))

(tm-define (version-get-revision u)
  (and (url-rooted-tmfs? u)
       (with (class name) (tmfs-decompose-name u)
         (tmfs-car name))))

(tm-define (version-get-revision* u)
  (if (version-revision? u)
      (version-get-revision u)
      ""))

(tm-define (version-head u)
  (and (url-rooted-tmfs? u)
       (with (class name) (tmfs-decompose-name u)
         (tmfs-string->url (tmfs-cdr name)))))

(tmfs-format-handler (revision name)
  (with u (tmfs-string->url (tmfs-cdr name))
    (url-format u)))

(tmfs-title-handler (revision name doc)
  (let* ((rev (tmfs-car name))
         (u (tmfs-string->url (tmfs-cdr name))))
    (string-append (url->system (url-tail u)) " - Revision " rev)))

(tm-define (version-revision name rev) "")
(tm-define (version-beautify-revision name rev) rev)

(tmfs-load-handler (revision name)
  (let* ((u (tmfs-string->url (tmfs-cdr name)))
         (tool (version-tool u)) ;; NOTE: forces lazy loading
         (rev (tmfs-car name)))
    (version-revision u rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating, registering and committing a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-supports-svn-style? name) (versioned? name))
(tm-define (version-supports-git-style? name) #f)

(tm-define (version-update name) "file is not under version control")
(tm-define (version-register name) "file is not under version control")
(tm-define (version-unregister name) "file is not under version control")
(tm-define (version-commit name comment) "file is not under version control")

(tm-define (update-buffer name)
  (let* ((old-stamp (url-last-modified name))
         (ret1 (version-update name))
         (ret2 (string-replace ret1 "\n" "; "))
         (new-stamp (url-last-modified name)))
    ;;(display* "ret2= " ret2 "\n")
    (when (> new-stamp old-stamp)
      (revert-buffer name))
    (set-message ret2 "Update buffer")))

(tm-define (register-buffer name)
  (let* ((ret1 (version-register name))
         (ret2 (string-replace ret1 "\n" "; ")))
    (set-message ret2 "Register file")))

(tm-define (commit-buffer-message name message)
  (let* ((ret1 (version-commit name message))
         (ret2 (string-replace ret1 "\n" "; "))
         (ret3 (if (!= ret2 "") ret2
                   "The current version has already been committed")))
    (set-message ret3 "Commit file")))

(tm-define (commit-buffer name)
  (interactive (lambda (message) (commit-buffer-message name message))))

(tm-define (version-interactive-update name)
  (:interactive #t)
  (save-buffer name :update))

(tm-define (version-interactive-commit name)
  (:interactive #t)
  (save-buffer name :commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control for wrapped urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-status name)
  (:require (== (version-tool name) "wrap"))
  (version-status (url-wrap name)))

(tm-define (version-history name)
  (:require (== (version-tool name) "wrap"))
  (version-history (url-wrap name)))

(tm-define (version-revision name rev)
  (:require (== (version-tool name) "wrap"))
  (version-revision (url-wrap name) rev))

(tm-define (version-beautify-revision name rev)
  (:require (== (version-tool name) "wrap"))
  (version-beautify-revision (url-wrap name) rev))

(tm-define (version-update name)
  (:require (== (version-tool name) "wrap"))
  (version-update (url-wrap name)))

(tm-define (version-register name)
  (:require (== (version-tool name) "wrap"))
  (version-register (url-wrap name)))

(tm-define (version-unregister name)
  (:require (== (version-tool name) "wrap"))
  (version-unregister (url-wrap name)))

(tm-define (version-commit name msg)
  (:require (== (version-tool name) "wrap"))
  (version-commit (url-wrap name) msg))

(tm-define (version-revision-url u rev)
  (:require (url-wrap u))
  (version-revision-url (url-wrap u) rev))

(tm-define (version-revision? u)
  (:require (url-wrap u))
  (version-revision? (url-wrap u)))

(tm-define (version-head u)
  (:require (url-wrap u))
  (version-head (url-wrap u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific customizations for parts of documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-revision-url u rev)
  (:require (url-rooted-tmfs-protocol? u "part"))
  (import-from (part part-tmfs))
  (let* ((name (part-open-name u))
         (m (part-master name))
         (f (part-file name)))
    (part-url (version-revision-url m rev) (version-revision-url f rev))))

(tm-define (version-head u)
  (:require (url-rooted-tmfs-protocol? u "part"))
  (import-from (part part-tmfs))
  (let* ((name (part-open-name u))
         (m (part-master name))
         (f (part-file name)))
    (part-url (version-head m) (version-head f))))
