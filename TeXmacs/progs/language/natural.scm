
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
;;   - Find a better way to gather strings to be translated than the hack in
;;     gui-markup.scm, which also doesn't require actual loading of the menus
;;   - Add plural forms and other tweaks to tr
;;   - Use TeXmacs to convert files with encodings different from that used by
;;     Guile (in tr-parse and ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language natural))

(define (replace-arg str arg val)
  (let ((i (string-contains str arg))
        (r (string-length arg)))
    (if i 
        (begin
          (if (number? val) (set! val (number->string val)))
          (if (list? val) (set! val (list->string val)))
          (if (symbol? val) (set! val (symbol->string val)))
          (string-append (substring str 0 i) 
                         val 
                         (string-drop str (+ i r))))
        str)))

(tm-define (tr origstr . vals)
  (with str (string-translate origstr)
    (with n 0
      (list-fold
       (lambda (val s)
         (set! n (+ n 1))
         (with arg (string-append "%" (number->string n))
           (replace-arg s arg val)))
       str vals))))

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
    (if r (display* (tr "Read %1 items from %2" (ahash-size h) f))
        (display* (tr "Error reading from file %1" f)))))

(define (tr-all language)
  "Merge the strings from the translations file with those misssing"
  (if (== 0 (ahash-size (tr-hash language))) (tr-parse language))
  (list-sort 
   (list-uniq
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
    (tr "This will overwrite the dictionary %1 with %2 entries. Are you sure?" 
        (tr-file language) (ahash-size (tr-hash language)))
    #f
    (lambda (answ)
      (if answ
          (begin
            (with-output-to-file (tr-file language)
              (lambda ()
                (tr-write (map (lambda (str) (tr-match language str))
                               (tr-all language)))))
            (set-message (tr "Wrote file %1" (tr-file language)) ""))
          (set-message (tr "Rebuild cancelled") language)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind translations-menu
  ("Edit translations file" (load-buffer (tr-file (get-output-language))))
  (if (buffer-exists? (tr-file (get-output-language)))
    (group "Close translations to rebuild"))
  (if (not (buffer-exists? (tr-file (get-output-language))))
    ("Rebuild translations file" (tr-rebuild (get-output-language))))
  ("Force reloading of translations"
   (force-load-translations "english" (get-output-language))))

