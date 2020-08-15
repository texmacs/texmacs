
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-navigate.scm
;; DESCRIPTION : navigation routines for links
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-navigate)
  (:use (utils library cursor) (link link-edit) (link link-extern)
        (generic generic-edit)))

; FIXME: remove these two and find a better way
(define (escape-link-args s)
  "Escape only args of the type #some:label"
  (let* ((m (string-length s))
         (h (or (string-index s #\#) m))
         (a (or (string-index s #\?) m))
         (i (min m h a))
         (base (substring s 0 i))
         (qry (substring s (min m i) m)))
    (if (< i m) (string-append base (string-replace qry ":" "%3A")) s)))

(define (unescape-link-args s)
  (string-replace s "%3A" ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define navigation-bidirectional-links? #f)
(define navigation-external-links? #t)
(define navigation-link-pages? #t)
(define navigation-blocked-types (make-ahash-table))

(define (navigation-bidirectional?) navigation-bidirectional-links?)
(tm-define (navigation-toggle-bidirectional)
  (:synopsis "Toggle whether we may follow links in both directions.")
  (:check-mark "v" navigation-bidirectional?)
  (set-boolean-preference "bidirectional navigation"
                          (not navigation-bidirectional-links?)))

(define (navigation-external?) navigation-external-links?)
(tm-define (navigation-toggle-external)
  (:synopsis "Toggle whether we may follow links defined in other loci.")
  (:check-mark "v" navigation-external?)
  (set-boolean-preference "external navigation"
                          (not navigation-external-links?)))

(define (navigation-build-link-pages?) navigation-link-pages?)
(tm-define (navigation-toggle-build-link-pages)
  (:synopsis "Toggle whether we generate link pages.")
  (:check-mark "v" navigation-build-link-pages?)
  (set-boolean-preference "link pages"
                          (not navigation-link-pages?)))

(define (navigation-allow-type? type)
  (not (ahash-ref navigation-blocked-types type)))
(tm-define (navigation-toggle-type type)
  (:synopsis "Toggle whether we may follow links of a given @type.")
  (:check-mark "v" navigation-allow-type?)
  (ahash-set! navigation-blocked-types type
              (not (ahash-ref navigation-blocked-types type))))

(define (navigation-allow-no-types?)
  (with l (ahash-table->list navigation-blocked-types)
    (null? (list-difference (current-link-types)
                            (map car (list-filter l cdr))))))
(tm-define (navigation-allow-no-types)
  (:synopsis "Disallow any link types from being followed.")
  (:check-mark "v" navigation-allow-no-types?)
  (for-each (cut ahash-set! navigation-blocked-types <> #t)
            (current-link-types)))

(define (navigation-allow-all-types?)
  (with l (ahash-table->list navigation-blocked-types)
    (null? (list-filter l cdr))))
(tm-define (navigation-allow-all-types)
  (:synopsis "Allow all link types to be followed.")
  (:check-mark "v" navigation-allow-all-types?)
  (set! navigation-blocked-types (make-ahash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-bidirectional-navigation var val)
  (set! navigation-bidirectional-links? (== val "on")))

(define (notify-external-navigation var val)
  (set! navigation-external-links? (== val "on")))

(define (notify-link-pages var val)
  (set! navigation-link-pages? (== val "on")))

(define-preferences
  ("bidirectional navigation" "off" notify-bidirectional-navigation)
  ("external navigation" "on" notify-external-navigation)
  ("link pages" "on" notify-link-pages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding links in the form of "link lists". Items in such a list
;;   (id type attrs vertex-1 ... vertex-n)
;; where id is the identifier of the locus which triggered the creation
;; of the list and type, attrs, vertex-1, ..., vertex-n contain
;; the link information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (link-item-id item) (car item))
(tm-define (link-item-type item) (cadr item))
(tm-define (link-item-attributes item) (caddr item))
(tm-define (link-item-vertices item) (cdddr item))

(define (id->link-list id)
  (let* ((lns (vertex->links `(id ,id)))
         (sts (map link-flatten lns)))
    (map (cut cons id <>) (map cdr sts))))

(tm-define (ids->link-list ids)
  (:synopsis "Build link list for identifiers in @ids")
  (append-map id->link-list ids))

(define (exact-link-list-global t)
  (ids->link-list (tree->ids t)))

(define (exact-link-list-local t)
  (if (not (tm-func? t 'locus)) '()
      (let* ((id (locus-id t))
             (lc (list-filter (cdr (tree-children t)) (cut tm-func? <> 'link)))
             (lns (map link-flatten lc)))
        (map (cut cons id <>) (map cdr lns)))))

(define (filter-on-bidirectional item)
  (== (link-item-id item)
      (vertex->id (car (link-item-vertices item)))))

(define (filter-on-type item)
  (navigation-allow-type? (link-item-type item)))

(define (filter-on-event event)
  (lambda (item)
    ;;(display* "Filter: " item " on " event "\n")
    (or (== event "click")
        (== (link-item-type item) event))))

(define (filter-link-list l event)
  (let* ((f1 (if (navigation-bidirectional?) l
                 (list-filter l filter-on-bidirectional)))
         (f2 (list-filter f1 filter-on-type))
         (f3 (list-filter f2 (filter-on-event event))))
    f3))

(tm-define (exact-link-list t filter?)
  (:synopsis "Build possibly filtered link list for the tree @t.")
  (:argument t "Build link list for this tree")
  (:argument filter? "Filter on navigation mode?")
  ;;(display* "t     : " t "\n")
  ;;(display* "local : " (exact-link-list-local  t) "\n")
  ;;(display* "global: " (exact-link-list-global t) "\n")
  (with l (if (and filter? (not (navigation-external?)))
              (exact-link-list-local t)
              (exact-link-list-global t))
    (if filter? (filter-link-list l "click") l)))

(tm-define (upward-link-list t filter?)
  (:synopsis "Build possibly filtered link list for @t and its ancestors.")
  (:argument t "Build link list for this tree and its ancestors")
  (:argument filter? "Filter on navigation mode?")
  (if (or (not t) (null? (tree->path t))) '()
      (with l (exact-link-list t filter?)
        (if (root? t) l
            (append l (upward-link-list (tree-up t) filter?))))))

(tm-define (complete-link-list t filter?)
  (:synopsis "Build possibly filtered link list for @t and its descendants.")
  (:argument t "Build link list for this tree and its descendants")
  (:argument filter? "Filter on navigation mode?")
  (with l (exact-link-list t filter?)
    (if (tree-atomic? t) l
        (with ls (map (cut complete-link-list <> filter?) (tree-children t))
          (apply append (cons l ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prospect for active links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (link-may-follow? t)
  (:synopsis "Does @t contain an active link?")
  (nnull? (upward-navigation-list t)))

(define (link-active-upwards-sub t active-ids)
  (if (or (not t) (null? (tree->path t))) '()
      (let* ((ids (tree->ids t))
             (add? (nnull? (list-intersection ids active-ids)))
             (r (if add? (list t) '())))
        (if (root? t) r
            (append r (link-active-upwards-sub (tree-up t) active-ids))))))

(tm-define (link-active-upwards t)
  (:synopsis "Return active ancestor trees for the tree @t.")
  (with l (upward-link-list t #t)
    (link-active-upwards-sub t (map link-item-id l))))

(tm-define (link-active-ids l)
  (:synopsis "Return list of identifiers in @l which admit an active link.")
  (with r (filter-link-list (ids->link-list l) "click")
    (list-remove-duplicates (map link-item-id r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation lists contain concrete propositions for link traversal.
;; One item is of the form
;;   (type attrs n source-id target-vertex)
;; where type and attrs are the type and the attributes of the link,
;; source-id the source identifier and the target vertex target-vertex
;; the n-th vertex of the link. In particular, for simple links,
;; n=1 for direct traversal and n=0 for inverse traversal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (navigation-type item) (car item))
(tm-define (navigation-attributes item) (cadr item))
(tm-define (navigation-pos item) (caddr item))
(tm-define (navigation-source item) (cadddr item))
(tm-define (navigation-target item) (car (cddddr item)))

(define (navigation-list-sub type attrs nr source l)
  (if (null? l) l
      (let* ((head (list type attrs nr source (car l)))
             (tail (navigation-list-sub type attrs (+ nr 1) source (cdr l))))
        (if (== source (vertex->id (car l))) tail
            (cons head tail)))))

(tm-define (navigation-list-simplify l)
  (with t (make-ahash-table)
    (for (x l)
      (with key (list (navigation-target x)
                      (navigation-type x)
                      (navigation-attributes x)
                      (navigation-pos x))
        (ahash-set! t key x)))
    (map cdr (ahash-table->list t))))

(tm-define (link-list->navigation-list l)
  (:synopsis "Transforms the link list @t into a navigation list")
  (if (null? l) l
      (let* ((item (car l))
             (source (link-item-id item))
             (type (link-item-type item))
             (attrs (link-item-attributes item))
             (vertices (link-item-vertices item))
             (h (navigation-list-sub type attrs 0 source vertices))
             (r (link-list->navigation-list (cdr l)))
             (nl (navigation-list-simplify (append h r))))
        nl)))

(tm-define (upward-navigation-list t)
  (link-list->navigation-list (upward-link-list t #t)))

(tm-define (navigation-list-filter l type nr jumpable?)
  (cond ((null? l) l)
        ((and (or (== type #t) (== (navigation-type (car l)) type))
              (or (== nr #t) (== (navigation-pos (car l)) nr))
              (or (not jumpable?)
                  (func? (navigation-target (car l)) 'id 1)
                  (func? (navigation-target (car l)) 'url 1)
                  (func? (navigation-target (car l)) 'script)))
         (cons (car l) (navigation-list-filter (cdr l) type nr jumpable?)))
        (else (navigation-list-filter (cdr l) type nr jumpable?))))

(tm-define (navigation-list-types l)
  (list-remove-duplicates (map navigation-type l)))

(tm-define (navigation-list-xtypes l)
  (let* ((direct (navigation-list-filter l #t 1 #t))
         (inverse (navigation-list-filter l #t 0 #t))
         (dtypes (map navigation-type direct))
         (itypes (map (cut string-append <> "*")
                      (map navigation-type inverse))))
    (list-remove-duplicates (append dtypes itypes))))

(tm-define (navigation-list-first-xtype l xtype)
  (let* ((inverse? (string-ends? xtype "*"))
         (type (if inverse? (string-drop-right xtype 1) xtype))
         (fl (navigation-list-filter l type (if inverse? 0 1) #t)))
    (and (nnull? fl) (navigation-type fl))))

(define (resolve-navigation-list l fun)
  (if (null? l) (fun)
      (let* ((id (vertex->id (navigation-target (car l))))
             (ok (or (nstring? id) (nnull? (id->trees id)))))
        (if ok (resolve-navigation-list (cdr l) fun)
            (begin
              (resolve-id id)
              (delayed (:idle 25) (resolve-navigation-list (cdr l) fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (automatic-link-text back)
  (cond ((func? back 'id 1)
         (with ts (id->trees (vertex->id back))
           (and (nnull? ts) (tree->stree (car ts)))))
        ((func? back 'url 1)
         `(verbatim ,(vertex->url back)))
        ((func? back 'script)
         `(verbatim ,(vertex->script back)))
        (else #f)))

(tm-define (automatic-link back . opt)
  (let* ((broken-text (if (null? opt) "Broken" (car opt)))
         (id (create-unique-id))
         (text (automatic-link-text back)))
    (if (not text) `(with "color" "red" ,broken-text)
        `(locus (id ,id) (link "automatic" (id ,id) ,back) ,text))))

(tm-define (build-enumeration l)
  (if (<= (length l) 1) l
      `((enumerate
         (document
          ,@(map (lambda (x) `(surround (item) "" ,x)) l))))))

(define (navigation-item->document item)
  (automatic-link (navigation-target item)))

(define (navigation-list-by-type->document type l)
  (cons `(strong ,type)
        (build-enumeration (map navigation-item->document l))))

(define (navigation-list->document style l)
  (let* ((direct (navigation-list-filter l #t 1 #t))
         (inverse (navigation-list-filter l #t 0 #t))
         (direct-types (navigation-list-types direct))
         (inverse-types (navigation-list-types inverse))
         (direct-by-type (map (cut navigation-list-filter direct <> #t #f)
                              direct-types))
         (inverse-by-type (map (cut navigation-list-filter inverse <> #t #f)
                               inverse-types)))
    `(document
      (style ,style)
      (body (document
             (strong "Source")
             ,(automatic-link `(id ,(navigation-source (car l)))
                              "Unaccessible")
             ,@(append-map
                navigation-list-by-type->document
                (map (cut string-append "Direct " <>) direct-types)
                direct-by-type)
             ,@(append-map
                navigation-list-by-type->document
                (map (cut string-append "Inverse " <>) inverse-types)
                inverse-by-type))))))

(define (build-navigation-page-sub style l)
  (with doc (navigation-list->document style l)
    (open-auxiliary "Link page" doc)))

(tm-define (build-navigation-page l)
  (let* ((style `(tuple ,@(get-style-list)))
         (fun (lambda () (build-navigation-page-sub style l))))
    (resolve-navigation-list l fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jumping to different types of URLs is done in two stages. The load handler,
;; changing with the type of root of the url and the post-load handler, which
;; will tipically depend on the file format.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (url->item u)
  (with (base qry) (process-url u)
    (let* ((file (url->system u))
           (help? (and (== "texmacs-file" (file-format u)) 
                       (url-exists-in-help? file)))
           (text (if help? (help-file-title u) (basename (url->system base)))))
      ($link file text))))

(define (url-list->document l)
  ($tmdoc
    ($tmdoc-title "Link disambiguation")
    ($para "The link you followed points to several locations:")
    ($description-aligned
      (with c 0
        ($for (x l)
          ($describe-item 
              (number->string (begin (set! c (+ 1 c)) c)) (url->item x)))))))

(tm-define (build-disambiguation-page l)
  (open-auxiliary "Disambiguation page" (url-list->document l)))

; FIXME: many corner cases will get through 
;(inexistent relative paths for instance)
(define (default-filter-url u)
  (with (base qry) (process-url u)
    (or (== base "")
        (or (!= (url-root base) "default")
            (url-exists? base)))))

(define (default-root-disambiguator u)
 (with l (list-filter (url->list u) default-filter-url)
    (cond ((null? l) 
           (set-message `(verbatim ,(url->system u)) "Not found"))
          ((== 1 (length l)) (load-browse-buffer (car l)))
          (else (build-disambiguation-page l)))))

(define (process-url u)
  "Split a simple (not or'ed!!) url in base and query"
  (let* ((s (url->system u))
         (m (string-length s))
         (h (or (string-index s #\#) m))
         (a (or (string-index s #\?) m))
         (i (min m h a))
         (base (substring s 0 i))
         (qry (substring s (min m (+ 1 i)) m)))
    (if (< i m)  ; was there either a '?' or a '#' (with args)?
        (list (system->url (string-drop-right s (+ 1 (string-length qry))))
              (unescape-link-args qry))
        (list u ""))))

(define (texmacs-file-post qry)
  (if (and (string? qry) (not (string-null? qry)))
      (with dest (unescape-link-args qry)
        (go-to-label dest)
        (set-message (replace "At %1." dest) ""))))

(define generic-file-post texmacs-file-post)

(define (source-file-post qry)
  (let* ((lstr (or (query-ref qry "line") "0"))
         (cstr (or (query-ref qry "column") "0"))
         (sstr (or (query-ref qry "select") ""))
         (line (or (string->number lstr) 0))
         (column (or (string->number cstr) 0)))
    (go-to-line line)
    (go-to-column column)
    (set! column (or (select-word sstr (cursor-tree) column) 0))
    (set-message sstr (string-append lstr ":" (number->string column)))))

; FIXME? will this jump to HTML anchors?
(define html-file-post generic-file-post)

(define (default-post-handler u)
  (with (base qry) (process-url u)
    (with fm (file-format base)
      (cond ((== qry "") (noop))
            ((== fm "texmacs-file") (texmacs-file-post qry))
            ((== fm "generic-file") (generic-file-post qry))
            ((== fm "scheme-file") (source-file-post qry))
            ((== fm "cpp-file") (source-file-post qry))
            ((== fm "java-file") (source-file-post qry))
            ((== fm "scala-file") (source-file-post qry))
            ((== fm "python-file") (source-file-post qry))
            ((== fm "html-file") (html-file-post qry))
            (else
              (display* "Unhandled format for default queries: " fm "\n"))))))

(define (http-post-handler u)
  ; TODO: jump to anchors in HTML
  (noop))

(define (default-root-handler u)
  (cond ((and (url-rooted-protocol? u "default")
              (url-rooted-web? (current-buffer)))
         (default-root-handler (url-relative (current-buffer) u)))
        ((url-or? (url-expand u))
         (default-root-disambiguator (url-expand u)))
        (else (with (base qry) (process-url u)
                (if (!= "" (url->system base))
                    (load-browse-buffer base))))))

(define (tmfs-root-handler u)
  (load-browse-buffer u))

(define (http-root-handler u)
  (load-browse-buffer u))

(define (url-handlers u)
  (with root (or (and (url-rooted? u) (url-root u)) "default")
    (cond ((== root "default") 
           (list default-root-handler default-post-handler))
          ((== root "tmfs") 
           (list tmfs-root-handler (lambda (x) (noop))))
          ((== root "file") ;; TODO: to be refined
           (list http-root-handler http-post-handler))
          ((or (== root "http") (== root "https"))
           (list http-root-handler http-post-handler))
          (else (display* "Unhandled url root: " root "\n")
                (list default-root-handler default-post-handler)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-id id . opt-from)
  (:synopsis "Jump to the first locus with a given identifier @id.")
  (:argument opt-from "Optional path for the cursor history")
  (with l (id->trees id)
    (if (nnull? l)
        (begin
          (if (nnull? opt-from) (cursor-history-add (car opt-from)))
          (tree-go-to (car l) :end)
          (if (nnull? opt-from) (cursor-history-add (cursor-path))))
        (and (resolve-id id)
             (delayed (:idle 25) (apply go-to-id (cons id opt-from)))))))

(tm-define (go-to-url u . opt-from)
  (:synopsis "Jump to the url @u")
  (:argument opt-from "Optional path for the cursor history")
  (if (nnull? opt-from) (cursor-history-add (car opt-from)))
  (if (string? u) (set! u (system->url u)))
  (with (action post) (url-handlers u) 
    (action u) (post u))
  (if (nnull? opt-from) (cursor-history-add (cursor-path))))

(define (execute-at cmd opt-location)
  (if (null? opt-location) (exec-delayed cmd)
      (exec-delayed-at cmd (car opt-location))))

(tm-define (old-execute-script s secure-origin? opt-location)
  ;; NOTE: this code is deprecated; we should remove the old support
  ;; when we will be sure that nobody uses it anymore.
  (let* ((secure-s (string-append "(secure? '" s ")"))
         (ok? (or secure-origin? (eval (string->object secure-s))))
         (cmd-s (string-append "(lambda () " s ")"))
         (cmd (eval (string->object cmd-s))))
    (cond ((or ok? (== (get-preference "security") "accept all scripts"))
           (execute-at cmd opt-location))
          ((== (get-preference "security") "prompt on scripts")
           (user-confirm `(concat "Execute " ,s "?") #f
             (lambda (answ)
               (when answ (execute-at cmd opt-location)))))
          (else (set-message "Unsecure script refused" "Evaluate script")))))

(tm-define (new-execute-script s secure-origin? args)
  (let* ((sym-fun (eval (string->object (string-append "'" s))))
	 (sym-cmd (cons sym-fun (map (lambda (x) (list 'quote x)) args)))
         (ok? (or secure-origin? (secure? sym-cmd)))
	 (cmd (eval (list 'lambda (list) sym-cmd))))
    (cond ((or ok? (== (get-preference "security") "accept all scripts"))
           (exec-delayed cmd))
          ((== (get-preference "security") "prompt on scripts")
           (user-confirm `(concat "Execute " ,s "?") #f
             (lambda (answ)
               (when answ (exec-delayed cmd)))))
          (else (set-message "Unsecure script refused" "Evaluate script")))))

(tm-define (execute-script s secure-origin? . opt-location)
  (if (and (string-starts? s "(") (string-ends? s ")")
	   (not (string-starts? s "(lambda ")))
      (old-execute-script s secure-origin? opt-location)
      (new-execute-script s secure-origin? opt-location)))

(define (go-to-vertex v attrs)
  (cond ((func? v 'id 1)
         (go-to-id (cadr v) (cursor-path)))
        ((func? v 'url 1)
         (go-to-url (escape-link-args (cadr v)) (cursor-path)))
        ((func? v 'script)
         (with ok? (== (assoc-ref attrs "secure") "true")
           (apply execute-script (cons* (cadr v) ok? (cddr v)))))
        (else (noop))))

(define (vertex-linked-ids v)
  (let* ((lns (vertex->links v))
         (vs (list-difference (append-map link-vertices lns) (list v))))
    (list-remove-duplicates (filter-map vertex->id vs))))

(define (id-update id)
  (let* ((ts1 (id->trees id))
         (ts2 (id->trees (string-append "&" id)))
         (pl (filter-map tree->path (append ts1 ts2))))
    (for-each update-all-path pl)))

(define (id-set-visited id)
  (when (not (string-starts? id "%"))
    (declare-visited (string-append "id:" id)))
  (for-each id-update (cons id (vertex-linked-ids `(id ,id)))))

(define (url-set-visited url)
  (declare-visited (string-append "url:" url))
  (for-each id-update (vertex-linked-ids `(url ,url))))

(define (navigation-item-follow hit)
  (let* ((source (navigation-source hit))
         (target (navigation-target hit)))
    (id-set-visited source)
    (and-with target-id (vertex->id target)
      (id-set-visited target-id))
    (and-with target-url (vertex->url target)
      (url-set-visited target-url))
    (go-to-vertex target (navigation-attributes hit))))

(define the-navigation-list '())
(tm-define (navigation-list-follow-xtyped xtype)
  (:synopsis "Follow the first link with given @type in @the-navigation-list.")
  (:argument xtype "Link type")
  (:proposals xtype (navigation-list-xtypes the-navigation-list))
  (and-with hit (navigation-list-first-xtype the-navigation-list xtype)
    (set! the-navigation-list #f)
    (navigation-item-follow hit)))

(tm-define (navigation-list-follow nl)
  (:synopsis "Follow one of the links in the navigation list @nl.")
  (with types (navigation-list-types nl)
    (if (and (>= (length types) 2) (in? "automatic" types))
        (with auto-nl (navigation-list-filter nl "automatic" #t #f)
          (set! nl (list-difference nl auto-nl))))
    (with xtypes (navigation-list-xtypes nl)
      (cond ((null? xtypes) (noop))
            ((and (navigation-build-link-pages?) (>= (length nl) 2))
             (id-set-visited (navigation-source (car nl)))
             (build-navigation-page nl))
            ((null? (cdr xtypes)) (navigation-item-follow (car nl)))
            (else
             (set! the-navigation-list nl)
             (interactive navigation-list-follow-xtyped))))))

(tm-define (link-follow-ids ids event)
  (:synopsis "Follow one of the links for identifiers in @ids.")
  (navigation-list-follow
   (link-list->navigation-list
    (filter-link-list (ids->link-list ids) event))))

(tm-define (locus-link-follow)
  (:synopsis "Follow one of the links in the current locus.")
  (let* ((ts (link-active-upwards (cursor-tree)))
         (ids (append-map tree->ids ts)))
    (link-follow-ids ids "click")))
