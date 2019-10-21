
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : natural.scm
;; DESCRIPTION : Natural languages manipulation and translation
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; TODO:
;;   - Use a better way to gather strings to be translated than the hack in
;;     gui-markup.scm.
;;   - Add plural forms and other tweaks to replace
;;   - Use TeXmacs to convert files with encodings different from that used by
;;     Guile (in tr-parse and ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language natural))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reformat-arg val)
  (cond ((number? val) (number->string val))
        ((symbol? val) `(verbatim ,(symbol->string val)))
        ((tree? val) (tree->stree val))
        ((url? val) `(verbatim ,(url->string val)))
        (else val)))

; Recall that for menu and widget creation one must use the homonymous tag
; instead of this function
(tm-define (replace origstr . vals)
  (:synopsis "Translate a string with arguments")
  (tm->stree
   (translate (stree->tree `(replace ,origstr ,@(map reformat-arg vals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and writing dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tr-hashes (make-ahash-table))

(define (process k)
  "Clean one key in the hash all-translations of widget markup"
  (cond ((string? k) k)
        ((and (pair? k) (string? (cdr k)))
         (cdr k))
        ((pair? k) (process (cdr k)))
        (else k)))

(define (tr-current)
  "Return the translations that have been performed in the gui"
  (list-sort
   (list-filter
    (map (lambda (x) (process (car x)))
         (ahash-table->list all-translations))
    string?)
   string<?))

(tm-define (tr-file language)
  (:synopsis "Returns the path to the dictionary file for @language")
  (url-concretize
   (string-append "$TEXMACS_PATH/langs/natural/dic/english-"
                  language
                  ".scm")))

(define (tr-hash language)
  "Return the hash with translations for @language"
  (with h (ahash-ref tr-hashes language)
    (if h h 
        (ahash-set! tr-hashes language (make-ahash-table)))))

(define (tr-read port ahash)
  "Recursively read translated entries from the given @port into @ahash"
  (with fm (read port)
    (cond ((eof-object? fm) #t)
          ((and (pair? fm) (string? (car fm)) (string? (cadr fm)))
           (ahash-set! ahash (car fm) (cadr fm)) 
           (tr-read port ahash))
          (else #f))))

(define (tr-parse language)
  "Open and parse the translations file for @language into its hash"
  (let* ((f (tr-file language))
         (p (open-input-file f))
         (h (tr-hash language))
         (r (tr-read p h)))
    (close-input-port p)
    (if r (display* (replace "Read %1 items from %2" (ahash-size h) f))
        (display* (replace "Error reading from file %1" f)))))

(define (tr-all language)
  "Merge the strings from the translations file with those misssing"
  (if (== 0 (ahash-size (tr-hash language))) (tr-parse language))
  (list-sort 
   (list-remove-duplicates
    (append (tr-current) 
            (map car (ahash-table->list (tr-hash language)))))
   string<?))

(define (tr-match language str)
  "Return a list with @str and the translation for @str or \"\""
  (let* ((lstr (locase-all str))
         (trans (or (ahash-ref (tr-hash language) str)
                    (ahash-ref (tr-hash language) lstr))))
    (if (or (== trans #f) (== lstr trans) (== str trans))
        (list str "")
        (list str trans))))

(define (raw-write-string str)
  (map write-char (string->list str))
  '())

(define (tr-write l)
  (if (list? l)
      (map (lambda (x)
             (with s (string-append "(\"" (car x) "\" \"" (cadr x) "\")")
               (raw-write-string s) (newline))) l)
      (original-display l))
  '())

(tm-define (tr-rebuild language)
  (:synopsis "Rebuild translations file adding the missing ones (up to now)")
  (tr-parse language)
  (user-confirm
    `(replace 
       "This will overwrite the dictionary %1 with %2 entries. Are you sure?"
      (verbatim ,(tr-file language)) ,(ahash-size (tr-hash language)))
    #f
    (lambda (answ)
      (if answ
          (begin
            (with-output-to-file (tr-file language)
              (lambda ()
                (tr-write (map (lambda (str) (tr-match language str))
                               (tr-all language)))))
            (set-message `(replace "Wrote file %1"
                                   (verbatim ,(tr-file language)))
                         ""))
          (set-message '(replace "Rebuild cancelled") language)))))

(tm-define (tr-missing language)
  (:synopsis "Translations missing in the dictionary (up to now)")
  (if (== 0 (ahash-size (tr-hash language))) (tr-parse language))
  (list-fold
   (lambda (cur res) 
     (if (or (ahash-ref (tr-hash language) cur)
             (ahash-ref (tr-hash language) (locase-all cur)))
         res
         (cons cur res)))
   '()
   (tr-current)))

(tm-define (tr-reload-translations)
  (force-load-translations "english" (get-output-language))
  (set-message "Translations loaded" (get-output-language)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind translations-menu
  (when (!= "english" (get-output-language))
    ("Edit translations file" (load-document (tr-file (get-output-language))))
    (if (buffer-exists? (tr-file (get-output-language)))
        (group "Close translations to rebuild"))
    (if (not (buffer-exists? (tr-file (get-output-language))))
        ("Rebuild translations file" (tr-rebuild (get-output-language))))
    ("Force reloading of translations" (tr-reload-translations))))

