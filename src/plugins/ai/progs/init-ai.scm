
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
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("chatgpt-text-input" "on" noop)
  ("gemini-text-input" "on" noop)
  ("llama3-text-input" "on" noop)
  ("llama4-text-input" "on" noop)
  ("open-mistral-7b-text-input" "on" noop))

(tm-widget (plugin-preferences-widget name)
  (:require (in? name (list "chatgpt" "gemini" "llama3" "llama4"
                            "open-mistral-7b")))
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
;; Llama
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-llama? model)
  (and (url-exists-in-path? "ollama")
       (with val (eval-system "ollama list")
         (string-occurs? (string-append "\n" model) val))))

(plugin-configure llama3
  (:require (has-llama? "llama3"))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Llama 3")
  (:serializer ,ia-serialize))

(tm-define (has-llama4?)
  (url-exists-in-path? "ollama"))

(plugin-configure llama4
  (:require (has-llama? "llama4"))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Llama 4")
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
