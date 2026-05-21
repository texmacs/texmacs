
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-tools.scm
;; DESCRIPTION : various tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-tools))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (count-characters doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (string-length s)))

(define (compress-spaces s)
  (let* ((s1 (string-replace s "\n" " "))
         (s2 (string-replace s1 "\t" " "))
         (s3 (string-replace s2 "  " " "))
         (s4 (if (string-starts? s3 " ") (string-drop s3 1) s3))
         (s5 (if (string-ends? s4 " ") (string-drop-right s4 1) s4)))
    (if (== s5 s) s (compress-spaces s5))))

(tm-define (count-words doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (length (string-tokenize-by-char (compress-spaces s) #\space))))

(tm-define (count-lines doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (length (string-tokenize-by-char s #\newline))))

(define (selection-or-document)
  (if (selection-active-any?)
      (selection-tree)
      (buffer-tree)))

(tm-define (statistics-scope-name)
  (if (selection-active-any?) "Selection" "Document"))

(tm-define (statistics-count-message scope kind nr)
  (string-append scope " " kind " count: " (number->string nr)))

(tm-define (statistics-summary-message scope characters words lines)
  (string-append scope " statistics: "
                 (number->string characters) " characters, "
                 (number->string words) " words, "
                 (number->string lines) " lines"))

(tm-define (show-statistics-summary)
  (let* ((doc (selection-or-document))
         (scope (statistics-scope-name))
         (characters (count-characters doc))
         (words (count-words doc))
         (lines (count-lines doc)))
    (set-message (statistics-summary-message scope characters words lines) "")))

(tm-define (show-character-count)
  (let* ((doc (selection-or-document))
         (scope (statistics-scope-name))
         (nr (count-characters doc)))
    (set-message (statistics-count-message scope "character" nr) "")))

(tm-define (show-word-count)
  (let* ((doc (selection-or-document))
         (scope (statistics-scope-name))
         (nr (count-words doc)))
    (set-message (statistics-count-message scope "word" nr) "")))

(tm-define (show-line-count)
  (let* ((doc (selection-or-document))
         (scope (statistics-scope-name))
         (nr (count-lines doc)))
    (set-message (statistics-count-message scope "line" nr) "")))

(define (save-aux-enabled?) (== (get-env "save-aux") "true"))
(tm-define (toggle-save-aux)
  (:synopsis "Toggle whether we save auxiliary data")
  (:check-mark "v" save-aux-enabled?)
  (let ((new (if (== (get-env "save-aux") "true") "false" "true")))
    (init-env "save-aux" new)))

(tm-define (toggle-show-kbd)
  (:synopsis "Toggle whether we show keyboard presses")
  (:check-mark "v" get-show-kbd)
  (set-show-kbd (not (get-show-kbd))))

(tm-define (clear-font-cache)
  (:synopsis "Clear font cache under TEXMACS_HOME_PATH")
  (map system-remove
    (list
      "$TEXMACS_HOME_PATH/system/cache/font_cache.scm"
      "$TEXMACS_HOME_PATH/fonts/font-database.scm"
      "$TEXMACS_HOME_PATH/fonts/font-features.scm"
      "$TEXMACS_HOME_PATH/fonts/font-characteristics.scm")))

(tm-define (scan-disk-for-fonts)
  (:interactive #t)
  (:synopsis "Scan disk for more fonts")
  (system-wait "Full search for more fonts on your system"
               "(can be long)")
  (font-database-build-local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (picture-gc)
  (picture-cache-reset)
  (update-all-buffers))
