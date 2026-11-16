
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-widgets.scm
;; DESCRIPTION : widgets for ai
;; COPYRIGHT   : (C) 2026  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tools ai ai-widgets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer for the explantions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-correct-buffer)
  (string->url "tmfs://aux/ai-correct"))

(define (focus-on-master-buffer)
  (buffer-focus* (buffer-get-master (ai-correct-buffer))))

(define (focus-on-master-buffer-cb cb)
  (with b (buffer-get-master (ai-correct-buffer))
    (when (buffer-exists? b)
      (buffer-focus* b)
      (cb))))

(define (explain-document t)
  `(with "bg-color" "#eee" 
     (tabular
      (tformat (cwith "1" "1" "1" "1" "cell-width" "1par")
               (cwith "1" "1" "1" "1" "cell-hmode" "exact")
               (cwith "1" "1" "1" "1" "cell-hyphen" "t")
               (table (row (cell ,t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main widget for ai-corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((ai-correct-widget) quit)
  (padded
    (vlist
      (hlist
        (explicit-buttons ("Correct"
                          (focus-on-master-buffer-cb (lambda () 
			    (ai-correct (get-preference "ai")))))) //
        (text "using agent: ") 
            (enum (set-preference "albert ai-agents corrector" answer)
                  (cons "default" (ai-agents-correctors))
                  (get-preference "albert ai-agents corrector") "11em")
        >>> //
        ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
         (set-boolean-preference "toolbar ai-correct" #t)
	 (focus-on-master-buffer-cb
	  (lambda () (open-ai-correct-toolbar*)))
	 (quit))
        ((balloon (icon "tm_close_tool.xpm") "Close")
         (focus-on-master-buffer-cb
	  (lambda () (version-retain-all 'current-old)))
         (quit)))
      === ===
      (refreshable "ai-widget-explanation"
	(texmacs-output (buffer->tree (ai-correct-buffer))
			`(style "generic"))))))

(tm-define (open-ai-correct-widget*)
  (dialogue-window (ai-correct-widget)
		   noop "Correction comments from AI" (ai-correct-buffer)))

(tm-define (open-ai-correct-widget doc)
  (let* ((u (current-buffer))
         (aux (ai-correct-buffer)))
    (buffer-set-master aux u)
    (buffer-set-body aux (explain-document doc))
    (open-ai-correct-widget*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toolbar for ai-corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exit-toolbar)
  (set! toolbar-ai-correct-active? #f)
  (update-bottom-tools)
  (when toolbar-db-active?
    (db-show-toolbar))
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)))

(tm-widget (ai-correct-toolbar)
  (padded
    (vlist
      (hlist
        (explicit-buttons
	  ("Correct"
	   (set! toolbar-ai-correct-active? #f)
	   (update-bottom-tools)
	   (update-menus)
	   (delayed (:idle 100) (focus-on-master-buffer-cb
	    (lambda () (ai-correct (get-preference "ai"))))))) //
        (text "using agent: ") 
            (enum (set-preference "albert ai-agents corrector" answer)
                  (cons "default" (ai-agents-correctors))
                  (get-preference "albert ai-agents corrector") "11em")
        >>> //
        ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
	 (set-boolean-preference "toolbar ai-correct" #f)
	 (exit-toolbar)
	 (open-ai-correct-widget*))
        ((balloon (icon "tm_close_tool.xpm") "Close")
         (focus-on-master-buffer-cb
	  (lambda () (version-retain-all 'current-old)))
	  (exit-toolbar))))
      === ===
      (refreshable "ai-toolbar-explanation"
	(texmacs-output (buffer->tree (ai-correct-buffer))
			`(style "generic")))))

(tm-define (open-ai-correct-toolbar*)
  (set! toolbar-ai-correct-active? #f)
  (update-bottom-tools)
  (update-menus)
  (set-boolean-preference "toolbar ai-correct" #t)
  (set! toolbar-ai-correct-active? #t)
  (update-bottom-tools)
  (update-menus))

(tm-define (open-ai-correct-toolbar doc)
  (let* ((u (current-buffer))
         (aux (ai-correct-buffer)))
    (buffer-set-master aux u)
    (buffer-set-body aux (explain-document doc))
    (refresh-now "ai-toolbar-explanation")
    (open-ai-correct-toolbar*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-ai-correct doc)
  (if (get-boolean-preference "toolbar ai-correct")
      (open-ai-correct-toolbar doc)
      (open-ai-correct-widget doc)))
