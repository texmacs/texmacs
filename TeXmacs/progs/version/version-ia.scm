
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-ia.scm
;; DESCRIPTION : asynchroneous AI functionality
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-ia)
  (:use (utils library process)
        (version version-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-translation-agent src-lan dest-lan)
  (string-append "Please translate the following HTML snippet from "
                 src-lan " into " dest-lan ", without explanations: "))

(define ((translate-processed-one serial tp st model next) out)
  (display* "------------------------------------------------------------\n")
  (display* out "\n")
  (display* "------------------------------------------------------------\n")
  (cond ((not (process-active? serial)) (noop))
        ((and (tm? out) (tree-pointer->tree tp))
         (let* ((t (tree-pointer->tree tp))
                (st* (tm->stree t))
                (out* (cpp-ai-output out model))
                (out** (cpp-ai-get-body out*))
                (new-t (decompress-html (car out**) 1)))
           (when (== st st*)
             (tree-set! t new-t)
             (tree-correct-upwards t)))))
  (tree-pointer-detach tp)
  (next))

(define ((translate-process-one dest-lan) serial tp st next)
  (let* ((t (tree-pointer->tree tp))
         (in (compress-html t 1))
         (src-lan (tree-get-env t "language"))
         (model (get-preference "ai"))
         (agent (ai-translation-agent src-lan dest-lan))
         (return (translate-processed-one serial tp st model next)))
    (if (process-running? 'translate)
        (let* ((cmd (cpp-ai-command in model agent))
               (cmd* (string-append cmd " 2> /dev/null")))
          (display* "Eval] " cmd "\n")
          (async-eval-system cmd* return))
        (return in #f))))

(define (make-translate-process lan)
  (make-process (translate-process-one lan) 'translate))

(tm-define (ia-translate* lan)
  ((make-translate-process lan)))

(tm-define (ia-abort-translate)
  (process-deactivate 'translate))
