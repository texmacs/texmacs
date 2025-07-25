
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai.scm
;; DESCRIPTION : AI tools
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc ai))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre- and post-processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ia-serialize lan t)
  (when (tm-func? t 'document 1)
    (set! t (tm-ref t 0)))
  (if (tm-atomic? t)
      (with s (tm->string t)
        (cork->utf8 s))
      (with s (convert (tm->stree t) "texmacs-stree" "latex-snippet"
                       (cons "texmacs->latex:encoding" "utf-8"))
        ;;(display* "s = " s "\n")
        s)))

(tm-define (ai-cmdline name cmd)
  (cpp-ai-latex-command cmd name))

(tm-define (ai-result name res)
  (cpp-ai-latex-output res name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-correct model)
  (when (selection-active-any?)
    (with lan (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (cpp-ai-correct t lan model)
          (insert r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-translate into model)
  (when (selection-active-any?)
    (with from (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (cpp-ai-translate t from into model)
          (insert r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste while compressing non natural language text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-copy)
  (when (selection-active-any?)
    (clipboard-set "primary" (compress-html (selection-tree) 1))))

(tm-define (ai-cut)
  (when (selection-active-any?)
    (ai-copy)
    (clipboard-cut "dummy")))

(define (clipboard-get* key)
  (with t (clipboard-get key)
    (cond ((not (tm-func? t 'tuple)) t)
          ((< (tm-arity t) 2) t)
          ((tm-equal? (tm-ref t 0) "texmacs")
           (tm->string (tm-ref t 1)))
          ((tm-equal? (tm-ref t 0) "extern")
           (tm->string (tm-ref t 1)))
          (else t))))

(tm-define (ai-paste)
  (with t (decompress-html (clipboard-get* "extern") 1)
    (insert t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently supported models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-chatgpt?)
  (and (url-exists-in-path? "openai")
       (!= (getenv "OPENAI_API_KEY") "")))

(plugin-configure chatgpt
  (:require (has-chatgpt?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:session "ChatGPT")
  (:serializer ,ia-serialize))

(tm-define (has-llama3?)
  (url-exists-in-path? "ollama"))

(plugin-configure llama3
  (:require (has-llama3?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:session "Llama 3")
  (:serializer ,ia-serialize))

(tm-define (has-open-mistral-7b?)
  (!= (getenv "MISTRAL_API_KEY") ""))

(plugin-configure open-mistral-7b
  (:require (has-open-mistral-7b?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:session "Mistral 7B")
  (:serializer ,ia-serialize))
