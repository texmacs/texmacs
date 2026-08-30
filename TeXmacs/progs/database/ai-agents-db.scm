
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-agents-db.scm
;; DESCRIPTION : AI agents database
;; COPYRIGHT   : (C) 2026  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database ai-agents-db)
  (:use (database db-convert)
        (database db-edit)))

(tm-define (ai-agents-database) (user-database "ai-agents"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formats of AI agents entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-kind-table
  ("ai-agents" ("corrector" "interlocutor" "translator")))

(smart-table db-format-table
  ("corrector"
   (and "instructions")) 
  ("interlocutor"
   (and "instructions")) 
  ("translator"
   (and "instructions")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load and save agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ai-agents-types-list
  (smart-ref db-kind-table "ai-agents"))

(tm-define (ai-agents-load)
  (db-load-types ai-agents-types-list))

(tm-define (ai-agents-save t)
  (db-save-types t ai-agents-types-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-agents-correctors)
  (with-database (ai-agents-database)
    (with ids (db-search `(("type" "corrector")))
      (map (lambda (id) (car (db-get-field id "name"))) ids))))

(tm-define (ai-agents-interlocutors)
  (with-database (ai-agents-database)
    (with ids (db-search `(("type" "interlocutor")))
      (map (lambda (id) (car (db-get-field id "name"))) ids))))

(tm-define (ai-agents-translators)
  (with-database (ai-agents-database)
    (with ids (db-search `(("type" "translator")))
      (map (lambda (id) (car (db-get-field id "name"))) ids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define builtin-corrector
  "Correct spelling, grammar, and improve the writing style of scientific documents.")

(define builtin-interlocutor "")

(define builtin-translator "")

(tm-define (ai-agents-get-corrector engine)
  (with-database (ai-agents-database)
    (with name (get-preference
		(string-append engine " ai-agents corrector"))
      (if (== name "default") builtin-corrector
	  (if (in? name (ai-agents-correctors))
	      (with ids (db-search `(("type" "corrector") ("name" ,name)))
		(if (null? ids) ""
		    (with instructions (db-get-field (car ids) "instructions")
		      (if instructions (car instructions) ""))))
	      (begin
		(db-warning "AI corrector agent '" name "' not found")
		(set-preference
		 (string-append engine " ai-agents corrector") "default")
		builtin-corrector))))))

(tm-define (ai-agents-get-interlocutor engine)
  (with-database (ai-agents-database)
    (with name (get-preference
		(string-append engine " ai-agents interlocutor"))
      (if (== name "default") builtin-interlocutor
	  (if (in? name (ai-agents-interlocutors))
	      (with ids (db-search `(("type" "interlocutor") ("name" ,name)))
		(if (null? ids) ""
		    (with instructions (db-get-field (car ids) "instructions")
		      (if instructions (car instructions) ""))))
	      (begin
		(db-warning "AI interlocutor agent '" name "' not found")
		(set-preference
		 (string-append engine " ai-agents interlocutor") "default")
		builtin-interlocutor))))))

(tm-define (ai-agents-get-translator engine)
  (with-database (ai-agents-database)
    (with name (get-preference
		(string-append engine " ai-agents translator"))
      (if (== name "default") builtin-translator
	  (if (in? name (ai-agents-translators))
	      (with ids (db-search `(("type" "translator") ("name" ,name)))
		(if (null? ids) ""
		    (with instructions (db-get-field (car ids) "instructions")
		      (if instructions (car instructions) ""))))
	      (begin
		(db-warning "AI translator agent '" name "' not found")
		(set-preference
		 (string-append engine " ai-agents translator") "default")
		builtin-translator))))))
