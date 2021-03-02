
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file-menu.scm
;; DESCRIPTION : the file menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus file-menu)
  (:use
    (utils library cursor)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-files)
    (texmacs menus print-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menu for existing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (buffer-list-menu l)
  (for (name l)
    (let* ((abbr (buffer-get-title name))
           (abbr* (if (== abbr "") (url->system (url-tail name)) abbr))
           (mod? (buffer-modified? name))
           (short-name `(verbatim ,(string-append abbr* (if mod? " *" ""))))
           (long-name `(verbatim ,(url->system name))))
      ((check (balloon (eval short-name) (eval long-name)) "v"
              (== (current-buffer) name))
       (switch-to-buffer name)))))

(tm-define (buffer-more-recent? b1 b2)
  (>= (buffer-last-visited b1)
      (buffer-last-visited b2)))

(tm-define (buffer-sorted-list)
  (with l (list-filter (buffer-list) buffer-in-menu?)
    (list-sort l buffer-more-recent?)))

(tm-define (buffer-menu-list nr)
  (let* ((l1 (list-filter (buffer-list) buffer-in-menu?))
         (l2 (list-sort l1 buffer-more-recent?)))
    (sublist l2 0 (min (length l2) nr))))

(tm-define (buffer-go-menu)
  (let* ((l1 (list-difference (buffer-menu-list 15) (linked-file-list)))
         (l2 (map window->buffer (window-list)))
         (l3 (list-difference l2 (list (current-buffer)))))
    (buffer-list-menu (list-difference l1 l3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menu for recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (short-menu-name u)
  (if (not (url-rooted-tmfs? u))
      ;;(string-append (url->system (url-tail u)) " [" (url->system u) "]")
      (url->system (url-tail u))
      (tmfs-title u `(document ""))))

(define (long-menu-name u)
  (url->system u))

(tm-menu (file-list-menu l win?)
  (for (name l)
    (let* ((short-name `(verbatim ,(short-menu-name name)))
           (long-name `(verbatim ,(long-menu-name name))))
      ((balloon (eval short-name) (eval long-name))
       (if win? (load-document name) (load-buffer name))))))

(tm-define (recent-file-list nr)
  (let* ((l1 (map cdar (learned-interactive "recent-buffer")))
         (l2 (map unix->url l1))
         (l3 (list-filter l2 buffer-in-recent-menu?)))
    (sublist l3 0 (min (length l3) nr))))

(tm-define (recent-unloaded-file-list nr)
  (let* ((l1 (map cdar (learned-interactive "recent-buffer")))
         (l2 (map unix->url l1))
         (l3 (list-filter l2 buffer-in-recent-menu?))
         (dl (list-difference l3 (buffer-list))))
    (sublist dl 0 (min (length dl) nr))))

(tm-define (recent-directory-list nr)
  (let* ((l1 (recent-file-list nr))
         (l2 (map url-head l1))
         (l3 (list-remove-duplicates l2)))
    (list-filter l3 (cut url-rooted-protocol? <> "default"))))

(tm-define (recent-file-menu)
  (file-list-menu (recent-file-list 25) #t))

(tm-define (recent-unloaded-file-menu)
  (with l (list-difference (recent-unloaded-file-list 15) (linked-file-list))
    (file-list-menu l #f)))

(tm-define (linked-file-menu)
  (file-list-menu (list-remove-duplicates (linked-file-list)) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (import-menu flag?)
  (with l (converters-to-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (load-text (string-append "Load " (string-downcase name) " file"))
             (import-text `(concat "Import " ,name))
             (text (if flag? import-text name))
             (format (if (== fm "verbatim") "" fm)))
        ((eval text) (choose-file (buffer-importer fm) load-text format))))))

(tm-define (import-top-menu) (import-menu #t))
(tm-define (import-import-menu) (import-menu #f))

(tm-menu (export-menu flag?)
  (with l (converters-from-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (save-text (string-append "Save " (string-downcase name) " file"))
             (export-text `(concat "Export as " ,name))
             (text (if flag? export-text name)))
        ((eval text) (choose-file (buffer-exporter fm) save-text fm))))))

(tm-define (export-top-menu) (export-menu #t))
(tm-define (export-export-menu) (export-menu #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the File menu and for the icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind new-file-menu
  (if (window-per-buffer?)
      ("New window" (new-document)))
  (if (not (window-per-buffer?))
      ("New document" (new-document))
      ("New window" (new-document*)))
  ;;("Clone window" (clone-window))
  )

(menu-bind load-menu
  ("Load" (open-document))
  ("Revert" (revert-buffer))
  (if (not (window-per-buffer?))
      ("Load in new window" (open-document*)))
  ---
  (link import-top-menu)
  (if (nnull? (recent-file-list 1))
      ---
      (link recent-file-menu)))

(menu-bind export-as-image-menu
  ;; FIXME: no warning on overwrite!
  (for (fm (image-formats))
    ((eval (upcase-first fm))
     (choose-file export-selection-as-graphics
                  "Export selection as image" fm))))

(menu-bind save-menu
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer-as "Save TeXmacs file" "texmacs"))
  ---
  (link export-top-menu)
  ---
  ((eval '(concat "Export as " "Pdf"))
   (choose-file wrapped-print-to-file "Save pdf file" "pdf"))
  ((eval '(concat "Export as " "PostScript"))
   (choose-file wrapped-print-to-file "Save postscript file" "postscript"))
  (when (selection-active-any?)
    (=> "Export selection as image"
        (link export-as-image-menu))))

(menu-bind print-menu-sub
  (if (has-printing-cmd?)
      ("Print buffer" (print-buffer))
      ("Print page selection" (interactive print-pages)))
  ("Print buffer to file"
   (choose-file print-to-file "Print all to file" "postscript"))
  ("Print page selection to file"
   (interactive choose-file-and-print-page-selection)))

(menu-bind print-menu
  ("Preview" (preview-buffer))
  (if (use-print-dialog?)
      (if (has-printing-cmd?) ("Print" (print-buffer)))
      ("Print to file"
       (choose-file print-to-file "Print all to file" "postscript")))
  (if (not (use-print-dialog?))
      (-> "Print" (link print-menu-sub)))
  (if (use-menus?)
      (-> "Page setup" (link page-setup-menu)))
  (if (use-popups?)
      ("Page setup" (open-page-setup))))

(menu-bind print-menu-inline
  ("Preview" (preview-buffer))
  (if (use-print-dialog?)
      (if (has-printing-cmd?) ("Print" (print-buffer)))
      ("Print to file"
       (choose-file print-to-file "Print all to file" "postscript")))
  (if (not (use-print-dialog?))
      ---
      (link print-menu-sub)
      ---)
  (if (use-menus?)
      (-> "Page setup" (link page-setup-menu)))
  (if (use-popups?)
      ("Page setup" (open-page-setup))))

(menu-bind close-menu
  (if (window-per-buffer?)
      ("Close window" (close-document)))
  (if (not (window-per-buffer?))
      ("Close document" (close-document))
      ("Close window" (close-document*)))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The File menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind file-menu
  ("New" (new-document))
  ("Load" (open-document))
  ("Revert" (revert-buffer))
  (-> "Recent"
      (link recent-file-menu)
      (if (nnull? (recent-file-list 1)) ---)
      (when (nnull? (recent-file-list 1))
        ("Clear menu" (forget-interactive "recent-buffer"))))
  ---
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer-as "Save TeXmacs file" "texmacs"))
  ---
  (link print-menu)
  ---
  (-> "Import"
      (link import-import-menu))
  (-> "Export"
      (link export-export-menu)
      ---
      ("Pdf" (choose-file wrapped-print-to-file "Save pdf file" "pdf"))
      ("Postscript"
       (choose-file wrapped-print-to-file "Save postscript file" "postscript"))
      (when (selection-active-any?)
        (=> "Export selection as image"
            (link export-as-image-menu))))
  ---
  (if (window-per-buffer?)
      ("Close window" (close-document)))
  (if (not (window-per-buffer?))
      ("Close document" (close-document)))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Go menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind go-menu
  (when (cursor-has-history?)
    ("Back" (cursor-history-backward)))
  (when (cursor-has-future?)
    ("Forward" (cursor-history-forward)))
  ("Save position" (cursor-history-add (cursor-path)))
  ---
  (link buffer-go-menu)
  (if (nnull? (linked-file-list))
      ---
      (link linked-file-menu))
  (if (not (window-per-buffer?))
      (if (nnull? (recent-unloaded-file-list 1))
          ---
          (link recent-unloaded-file-menu)))
  (if (nnull? (bookmarks-menu))
      ---
      (link bookmarks-menu))
  (if (window-per-buffer?)
      ---
      (group "Buffer in this window")
      ("New" (new-document*))
      ("Load" (open-document*))
      (-> "Recent"
          (if (nnull? (recent-unloaded-file-list 1))
              ---
              (link recent-unloaded-file-menu)))
      ("Close" (close-document*))))
