
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : developer-menu.scm
;; DESCRIPTION : Menu items for developer mode
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; Things to do:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus developer-menu))

(use-modules (prog scheme-tools) (prog scheme-menu)
             (doc apidoc) (doc apidoc-widgets)
             (language natural)
             (utils misc gui-utils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous extra routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scm-load-buffer u)
   (load-document u) 
   (if (not (url-exists? u)) 
;; save empty file & reload so that it is recognized as scheme code, not plain tm doc  
      (begin (buffer-save u) (revert-buffer-revert))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (notify-keyboard-tool var val)
  (update-bottom-tools))

(define custom-keyboard-magnification "1.000")

(define-preferences
  ("custom keyboard" "" (lambda args (noop)))
  ("keyboard tool" "off" notify-keyboard-tool))

(tm-define (set-custom-keyboard-magnification mag)
  (if (!= custom-keyboard-magnification mag)
      (begin
        (set! custom-keyboard-magnification mag)
      )))

(tm-define (get-custom-keyboard-magnification)
  custom-keyboard-magnification)

(tm-define (set-custom-keyboard kbd)
  (with s (serialize-texmacs-snippet kbd)
    (set-preference "custom keyboard" s)))

(tm-define (get-custom-keyboard)
  (with s (get-preference "custom keyboard")
    (parse-texmacs-snippet s)))

(tm-define (get-the-keyboard)
  (with s (get-custom-keyboard)
    (if (not (tm-equal? s "")) s
        (get-keyboard))))

(tm-menu (custom-keyboard-toolbar)
  (hlist (glue #f #f 0 200)
    >>
    (texmacs-output
     `(with "bg-color" "#404040"
            "magnification" ,(get-custom-keyboard-magnification)
            ,(get-the-keyboard))
     '(style "new-gui"))
    >>))

(tm-define (has-custom-keyboard?)
  (== (get-preference "keyboard tool") "on"))

(tm-define (toggle-custom-keyboard)
  (:check-mark "*" has-custom-keyboard?)
  (with on? (not (has-custom-keyboard?))
    (set-boolean-preference "keyboard tool" on?)
    (refresh-now "custom-keyboard")))

(tm-widget (custom-keyboard-widget cmd)
  (refreshable "custom-keyboard"
    (invisible (get-the-keyboard))
    (texmacs-output
     `(with "bg-color" "#404040"
            "magnification" ,(get-custom-keyboard-magnification)
            ,(get-the-keyboard))
     '(style "new-gui"))))

;; Convertit une largeur de touche en nombre Scheme utilisable.
;; Le layout peut contenir soit un nombre, soit une chaine numerique.
;; Si la conversion echoue, on retourne une valeur par defaut.
(define (custom-keyboard-number x default)
  (cond ((number? x) x)
        ((string? x)
         (with v (string->number x)
           (if (number? v) v default)))
        (else default)))

;; Construit une commande executable pour une touche modificateur.
;; Exemple: key="shift" -> "(emu-toggle-modifier \"shift\")"
;; On renvoie une chaine de code Scheme, elle sera evaluee cote C++.
(define (custom-keyboard->command key)
  (string-append "(emu-toggle-modifier " (object->string key) ")"))

;; Convertit une touche du layout TeXmacs (std-key/extended-key/mod-key)
;; vers un format plus simple consomme par le clavier Qt:
;;   (width label cmd kind)
;; ou:
;;   width : nombre de colonnes relatives
;;   label : texte affiche sur le bouton
;;   cmd   : code Scheme a executer au clic
;;   kind  : metadata (std/extended/modifier), utile pour debug/evolutions
;;
;; Important:
;; - label et cmd sont gardes "bruts" (pas object->string), sinon on ajoute
;;   des guillemets et la commande devient une simple chaine non executable.
(define (custom-keyboard-entry t)
  (cond ((tm-func? t 'std-key 2)
         (let* ((label (tm->stree (tree-ref t 0)))
                (cmd   (tm->stree (tree-ref t 1))))
      (list 1.0 label cmd "std")))
        ((tm-func? t 'std-key 3)
         (let* ((label (tm->stree (tree-ref t 0)))
                (cmd   (tm->stree (tree-ref t 1)))
                (w     (custom-keyboard-number (tm->stree (tree-ref t 2)) 1.0)))
      (list w label cmd "std")))
        ((tm-func? t 'extended-key 3)
         (let* ((label (tm->stree (tree-ref t 0)))
                (cmd   (tm->stree (tree-ref t 1)))
                (w     (custom-keyboard-number (tm->stree (tree-ref t 2)) 1.0)))
      (list w label cmd "extended")))
        ((tm-func? t 'mod-key 2)
         (let* ((label (tm->stree (tree-ref t 0)))
                (w     (custom-keyboard-number (tm->stree (tree-ref t 1)) 1.0))
                (cmd   (custom-keyboard->command label)))
      (list w label cmd "modifier")))
        (else #f)))

;; Filtre les entrees invalides (#f) produites pendant le mapping.
;; Equivaut a un compact/map-filter.
(define (custom-keyboard-filter-valid l)
  (cond ((null? l) (list))
        ((car l)
         (cons (car l) (custom-keyboard-filter-valid (cdr l))))
        (else
         (custom-keyboard-filter-valid (cdr l)))))

;; Convertit une ligne "row" du clavier TeXmacs en liste de touches simplifiees.
;; Structure attendue cote TeXmacs:
;;   (row (cell (concat key1 key2 ...)))
;; Si la structure est differente, on renvoie une ligne vide.
(define (custom-keyboard-row row)
  (if (and (tm-func? row 'row 1)
           (tm-func? (tree-ref row 0) 'cell 1)
           (tm-func? (tree-ref (tree-ref row 0) 0) 'concat))
      (with parts (tree-children (tree-ref (tree-ref row 0) 0))
        (custom-keyboard-filter-valid (map custom-keyboard-entry parts)))
      (list)))

;; Resout recursivement les dynamic-case du clavier.
;; Le clavier de base est defini comme un arbre dynamique, par ex:
;;   (dynamic-case "fn" ... "no-fn" ...)
;;   (dynamic-case "shift" ... "no-shift" ...)
;;
;; Cette fonction choisit la bonne branche selon l'etat des modificateurs
;; (emu-active-modifier?) et retourne un arbre concret "keyboard".
;;
;; Note de robustesse:
;; get-the-keyboard peut renvoyer un tree ou un stree selon le contexte.
;; On normalise donc avec (stree->tree ...) avant tree-arity/tree-ref.
(define (custom-keyboard-resolve t)
  (with tt (if (tree? t) t (stree->tree t))
    (if (and (tm-func? tt 'dynamic-case) (>= (tree-arity tt) 4))
        (with mod (tm->stree (tree-ref tt 0))
          (if (and (string? mod) (emu-active-modifier? mod))
              (custom-keyboard-resolve (tree-ref tt 1))
              (custom-keyboard-resolve (tree-ref tt 3))))
        tt)))

;; Point d'entree appele par le code C++ Qt.
;; Retourne le layout courant du clavier sous forme "simple":
;;   ( (row1-key1 row1-key2 ...) (row2-key1 ...) ... )
;; ou chaque key = (width label cmd kind)
;;
;; Ce format evite de construire un widget Scheme complet cote C++ et permet
;; de creer directement des QPushButton natifs, plus fluides.
(tm-define (custom-keyboard-layout)
  (let* ((tree (custom-keyboard-resolve (get-the-keyboard))))
    (if (and (tm-func? tree 'keyboard 1)
             (tm-func? (tree-ref tree 0) 'tformat 1)
             (tm-func? (tree-ref (tree-ref tree 0) 0) 'table))
        (with rows (tree-children (tree-ref (tree-ref tree 0) 0))
          (map custom-keyboard-row rows))
        (list))))

(tm-define (open-custom-keyboard)
  (:interactive #t)
  (dialogue-window custom-keyboard-widget noop "Custom keyboard"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The developer menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind developer-menu
  (group "Scheme")
  (link scheme-menu)
  ---
  (group "Translations")
  (link translations-menu)
  ---
  (group "Documentation")
  (link apidoc-menu)
  ---
  (group "Configuration")
  ((replace "Open %1" (verbatim "my-init-texmacs.scm"))
   (scm-load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm")))
  ((replace "Open %1" (verbatim "my-init-buffer.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm")))
  ((replace "Open %1" (verbatim "preferences.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/system/preferences.scm")))
  ---
  (group "Custom keyboard")
  ("Show keyboard" (toggle-custom-keyboard))
  ("Open keyboard" (open-custom-keyboard))
  (when (selection-active-any?)
    ("Set keyboard" (set-custom-keyboard (tm->tree (selection-tree)))))
  (when (not (tm-equal? (get-custom-keyboard) ""))
    ("Reset keyboard" (set-custom-keyboard (tm->tree ""))))
  (assuming (side-tools?)
    ---
    (group "Experimental side tools")
    ("Reset left" (close-tools :left))
    ("Reset right" (close-tools :right))
    ("Buffer left" (tool-select :left 'buffer-tool))
    ("Buffer right" (tool-select :right 'buffer-tool))
    ("Context" (tool-select :right 'context-tool))
    ("Invalid" (tool-toggle :right 'invalid-tool))
    (-> "Test"
        ("Sections" (tool-select :right 'sections-tool))
        ("Subsections" (tool-select :right 'subsections-tool)))
    ;;(-> "Color"
    ;;    ("Color" (tool-select :right '(color-tool "Background color")))
    ;;    ("Pattern" (tool-select :right '(pattern-tool "Background pattern")))
    ;;    ("Gradient" (tool-select :right '(gradient-tool "Background gradient")))
    ;;    ("Picture" (tool-select :right '(picture-tool "Background picture"))))
    ))
