
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-ai.scm
;; DESCRIPTION : placeholder for various AI plugins
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ollama command line tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ollama-models)
  (let* ((ret (eval-system "ollama list"))
         (lines** (string-decompose ret "\n"))
         (lines* (if (null? lines**) lines** (cdr lines**)))
         (lines (if (and (nnull? lines*) (== (cAr lines*) ""))
                    (cDr lines*) lines*))
         (models (map (lambda (l) (car (string-decompose l " "))) lines)))
    (sort models string<=?)))

(tm-define (ollama-model-variants model)
  (with models (ollama-models)
    (append-map (lambda (m)
                  (if (string-starts? m model) (list m) (list)))
                models)))

(tm-define (ollama-default-model)
  (let* ((l (ollama-models))
         (llama (ollama-model-variants "llama")))
    (cond ((nnull? llama) (car llama))
          ((nnull? l) (car l))
          (else "llama3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("ollama server" "localhost" noop)
  ("ollama port" "11434" noop)
  ("ollama model" "default" noop)
  ("ollama-text-input" "on" noop)
  ("chatgpt-text-input" "on" noop)
  ("gemini-text-input" "on" noop)
  ("open-mistral-7b-text-input" "on" noop)
  ("albert-text-input" "on" noop)
  ("albert model" "openweight-large" noop))

(tm-define (ia-models)
  (list "chatgpt" "gemini" "open-mistral-7b" "albert" "ollama"))

(tm-define (albert-variants)
  (list "openweight-large" "openweight-medium" "openweight-small" ""))

(tm-widget (plugin-preferences-widget name)
  (:require (in? name (ia-models)))
  (assuming (== name "ollama")
    (aligned
      (item (text "Ollama server")
        (enum (set-preference "ollama server" answer) '("localhost" "")
              (get-preference "ollama server") "16em"))
      (item (text "Ollama port")
        (enum (set-preference "ollama port" answer) '("11434" "")
              (get-preference "ollama port") "16em"))
      (item (text "Ollama model")
        (enum (set-preference "ollama model" answer) (ollama-models)
              (get-preference "ollama model") "16em")))
    === === ===)
  (assuming (== name "albert")
    (with model (string-append name " model")
      (aligned
        (item (text model)
          (enum (set-preference model answer)
		(albert-variants)
                (get-preference model) "11em"))
	(item (text "Chat history size")
	  (enum (set-preference "albert chat history size" answer)
		'("10" "5" "4" "3" "2" "1" "0" "")
		(get-preference "albert chat history size") "6em"))))
    === === ===)
  (with textual-input (string-append name "-text-input")
    (aligned
      (meti (hlist // (text "Textual input"))
	(toggle (set-boolean-preference textual-input answer)
		(get-boolean-preference textual-input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-chatgpt?)
  (and (url-exists-in-path? "openai")
       (getenv "OPENAI_API_KEY")
       (!= (getenv "OPENAI_API_KEY") "")))

(plugin-configure chatgpt
  (:require (has-chatgpt?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "ChatGPT")
  (:serializer ,ia-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gemini
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-gemini?)
  (and (getenv "GEMINI_API_KEY")
       (!= (getenv "GEMINI_API_KEY") "")))

(plugin-configure gemini
  (:require (has-gemini?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Gemini")
  (:serializer ,ia-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ollama
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-ollama?)
  (url-exists-in-path? "ollama"))

(plugin-configure ollama
  (:require (has-ollama?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Ollama")
  (:serializer ,ia-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mistral
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-open-mistral-7b?)
  (and (getenv "MISTRAL_API_KEY")
       (!= (getenv "MISTRAL_API_KEY") "")))

(plugin-configure open-mistral-7b
  (:require (has-open-mistral-7b?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Mistral 7B")
  (:serializer ,ia-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Albert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-albert?)
  (and (getenv "ALBERT_API_KEY")
       (!= (getenv "ALBERT_API_KEY") "")))

(plugin-configure albert
  (:require (has-albert?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Albert")
  (:serializer ,ia-serialize))

