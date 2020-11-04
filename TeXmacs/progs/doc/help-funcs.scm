
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help-funcs.scm
;; DESCRIPTION : loading help files
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc help-funcs)
  (:use (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading help buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define help-file-path "$TEXMACS_DOC_PATH")
(define help-url-cache (make-ahash-table))
(define help-titles (make-ahash-table))
(define parse-times (make-ahash-table))

(define (parse-title u)
  (with format (cond ((== (url-suffix u) "tmml") "tmml")
                     (else "texmacs"))
   (with t (tree-import u format)
    (with tt (select t '(:* (:or title doc-title tmdoc-title 
                                 tmdoc-title* tmweb-title) :%1))
      (if (null? tt) '() (car tt))))))

(tm-define (help-file-title u)
  (let ((mod-time (url-last-modified u))
        (parse-time (or (ahash-ref parse-times u) 0)))
    (if (> mod-time parse-time) ; handy: false also if url invalid
        (begin
          (ahash-set! parse-times u mod-time)
          (ahash-set! help-titles u (parse-title u))))
    (ahash-ref help-titles u)))

(tm-define (url-exists-in-help? s)
  (with entry (ahash-ref help-url-cache s)
    (if (list? entry)
      (car entry)
      (car (ahash-set! help-url-cache s
                       (list (url-exists? (url-unix help-file-path s))))))))

(define (url-resolve-help s)
  (if (or (in? (url-suffix s) '("tex" "tm")) (url-exists? s))
      s
      (let* ((lan (string-take (language-to-locale (get-output-language)) 2)) 
             (suf-tmml (string-append "." lan ".tmml"))
             (suf (string-append "." lan ".tm"))
             (dir help-file-path))
        (cond ((url-exists? (url-unix dir (string-append s suf-tmml)))
               (url-resolve (url-unix dir (string-append s suf-tmml)) "r"))
              ((url-exists? (url-unix dir (string-append s suf)))
               (url-resolve (url-unix dir (string-append s suf)) "r"))
              ((and (!= suf ".en.tm")
                    (url-exists? (url-unix dir (string-append s ".en.tm"))))
               (url-resolve (url-unix dir (string-append s ".en.tm")) "r"))
              (else (url-none))))))

(define (load-help-buffer-sub s type)
  (let ((name (url-resolve-help s)))
    (cond ((url-none? name)
           (set-message
            `(concat "Error: help file " (verbatim ,s) " not found")
            "load help file"))
          ((== type "book") (tmdoc-expand-help-manual name))
          (else (tmdoc-expand-help name type)))))

(tm-define (load-help-buffer s) (load-help-buffer-sub s "normal"))
(tm-define (load-help-article s) (load-help-buffer-sub s "article"))
(tm-define (load-help-book s) (load-help-buffer-sub s "book"))

(tm-define (load-help-online s)
  (load-help-buffer (url-append "https://www.texmacs.org/tmbrowse" s)))

(tm-define (update-help-online)
  (system "cd $TEXMACS_HOME_PATH; wget ftp://ftp.texmacs.org/pub/TeXmacs/doc/TeXmacs-doc.tar.gz -O TeXmacs-doc.tar.gz; gunzip TeXmacs-doc.tar.gz; tar -xvf TeXmacs-doc.tar; rm -f TeXmacs-doc.tar"))
