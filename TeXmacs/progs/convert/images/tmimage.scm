;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmimage.scm
;; DESCRIPTION : convert texmacs fragment (selection) to image formats.
;;               Try embedding source code in image
;; COPYRIGHT   : (C) 2012  Philippe Joyez
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert images tmimage)
  (:use (convert tmml tmmlout)
        (convert tmml tmtmml)))

;; (display "Texmacs] Loading module tmimage\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert active selection to various graphics format
;; try embedding texmacs code of the selection in metadata of the image for
;; re-edition.
;; the svg produced by this method can be pasted in inkscape via the clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-image-export-format)
  (if (nnot (converter-search "pdf-file" "svg-file")) "svg" "pdf"))

(define-preferences
  ("texmacs->image:format" (default-image-export-format) noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; temporary files with extensions

(define (url-temp-ext ext)
  (url-glue (url-temp) (string-append "." ext)))

;; new system function call that check that output is produced
;; and give minimum information if not


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commodity functions for tree manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-node! node)
  ;; removes node (with children if any)
  (tree-remove! (tree-ref node :up) (tree-index node) 1))

(define (copy-node! node parent-dest pos)
  ;; insert an existing node (with children if any) as new child of parent-dest
  ;; FIXME: No sanity check! parent should not be in node's subtree!
  (tree-insert! parent-dest pos `(,node)))

(define (move-node! node parent-dest pos)
  ;; moves an existing node (with children if any) and
  ;; insert it as new child of parent-dest
  ;; FIXME: No sanity check! parent should not be in node's subtree!
  (copy-node! node parent-dest pos)
  (remove-node! node))

(define (remove-node-raise-children! node . firstlast)
  ;; similar to "remove tag" operation in the editor
  (let* ((parent (tree-ref node :up))
         (pos (+ (tree-index node) 1))
         (lastindex (if (< (length firstlast) 2) (- (tree-arity node) 1) (cadr firstlast) ))
         (firstindex (- (if (null? firstlast) 0 (car firstlast)) 1)))
    (do ((i lastindex (- i 1))) ((= i firstindex))
      (copy-node! (tree-ref node i) parent pos))
    (remove-node! node)
    ))

(define (replace-leaftext! leaf newtext)
  ;; replace a node's content by a new string.
  ;; Makes the node a leaf if it wasn't one
  (tree-assign! leaf (string->tree newtext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2 functions for remapping cross-referenced items (glyphs)
;; in the svg using unique ids. This is needed to avoid collisions between
;; definitions belonging to differents formulas in inkscape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (newids! lablist tm-fragment-string)
  ;; replaces all ids in the svg
  ;; plus returns an associationlist for mapping old id to new ones
  ;; we will use it to replace hyperlinks to the former ids to the new ones
  (let* ((unique (number->string (string-hash tm-fragment-string 1000000)))
         ;; generate a reproducible 6-digit number that depends
         ;; on the tm code of the selection
         (basename (string-append "tm" unique "-"))
         (newalist '())
         (n (length lablist)))
    (do ((i 0 (+ i 1))) ((= i n))
      (let* ((newlabel (string-append basename (number->string i)))
             (labelnode (list-ref lablist i))
             (oldlabel (tree->string labelnode)))
        (set! newalist (assoc-set! newalist (string-append "#" oldlabel)
                                   (string-append "#" newlabel) ))
        (replace-leaftext! labelnode newlabel)))
    newalist))

(define (replace-hlinks! hreflist alist)
  ;; use the above association list to actualy replace
  ;; the xlink:href items with updated targets
  (map (lambda (leaf)
         (let ((newtarget (assoc-ref alist (tree->string leaf))))
           (replace-leaftext! leaf newtarget)))
       hreflist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define latex and texmacs string representation of selection
;; we escape them to ascii so that they do not interfere with xml
;; <  -> &lt;  > -> &gt; \ -> \\, all characters above #127->\xXX ...
;; see  TeXmacs/langs/encodings/cork-escaped-to-ascii.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-encode tm-fragment-tree)
  ;; for the latex representation we mimick what is done when
  ;; "copy to latex" is performed
  (let* ((latex-tree (latex-expand tm-fragment-tree))
         ;; expand or not macros according to preferences
         (latex-code (texmacs->generic latex-tree "latex-snippet")))
         ;; actual conversion
    (escape-to-ascii latex-code)))

(define (tm-encode tm-fragment-tree)
  (escape-to-ascii (serialize-texmacs tm-fragment-tree)))

(define (remove-clip! clipg)
  (let* ((parent (tree-ref clipg :up))
         (pos (+ (tree-index clipg) 1))
         (lastindex (- (tree-arity clipg) 1)))
    (do ((i lastindex (- i 1))) ((= i 0))
      (let* ((node (tree-ref clipg i)))
             (move-node! node parent pos)))
    (remove-node! clipg)
    ))

(tm-define (cr2? s) (or (equal? (tree->stree s) "\n") (equal? (tree->stree s) "\n  ")))

(define (refactor-svg dest tm-fragment)
  ;; reorganize svg file and inject attributes containing tm code of
  ;; equation. dest is the url of the svg file to be edited
  ;; A latex fragment is also added for compatibility with
  ;; 'textext' inkscape extension
  ;; FIXME : no error checking, no return value...
  ;; for improvements (we could pass the style that was used when
  ;; the equation was created, the fonts,...)


  (let*
      (;; first: load svg and transform to an active tree in
       ;; temporary buffer so that we can manipulate it
       ;; using texmacs primitives for trees
       (svg-in (string-load dest)) ;; load svg file as string
       (s-svg-in (parse-xml svg-in)) ;; parse to stree
       (mybuf (buffer-new))
       ;; create temporary buffer for subsequent manipulations of svg tree
       (void (buffer-set-body mybuf (tree-assign-node! (stree->tree s-svg-in)
                                                       'concat)))
       ;; populate buffer with tree
       ;; replace *TOP* node by concat otherwise displaying
       ;; that buffer crashes texmacs

       ;; second: define a bunch of locations in the tree
       (buftree (buffer-get-body mybuf)) ;; the whole tree
       (svgroot (car (select buftree '(:* svg)))) ;; the <svg > node
       (maingroup (car (select svgroot '(g))))
       ;; the main group in the svg, containing the drawing layout
       (maingroup-attrib (car (select maingroup '(@))))
       ;; attributes of the main group
       (defs (car (select svgroot '(defs))))
       ;; the defs, containing the glyph vector outlines,
       ;; hyperlinked from the drawing (a.k.a cloned)
       (idlist (select svgroot '(:* id :%1)))
       ;; list of all ids in the drawing, used to label glyph outlines
       (hreflist (select maingroup '(:* @ xlink:href :%1)))
       ;; list of hyperlinks to the glyphs labels

       ;; third: the new data we want to insert in the tree
       (latex-code (latex-encode tm-fragment))
       (tm-code (tm-encode tm-fragment))
       (tm-style (tm-encode (get-all-inits)))
       ;; define new attributes containing latex and texmacs code:
       (extra-latex-attrib
        `((xmlns:ns0 "http://www.iki.fi/pav/software/textext/")
          (ns0:text ,latex-code) (ns0:preamble "texmacs_latex.sty")))
       (extra-tm-attrib `((xmlns:ns1 "https://www.texmacs.org/")
                          (ns1:texmacscode ,tm-code) (ns1:texmacstyle ,tm-style)))
       ;; OK, the texmacs namespace maybe not correctly described at that url
       (old->new-labels (newids! idlist tm-code))
       ;; rename all ids, create an association list of old to new ids
       )

    ;; fourth: modify tree
    (replace-hlinks! hreflist old->new-labels)
    ;; replace hlinks with new pointers
    (map remove-node! (select svgroot '(:* (:match :cr2?))))
    (map remove-clip! (reverse (select maingroup '(:* g @ clip-path :up :up))))
    (map remove-node! (select defs '(:* clipPath) ))
    ;; cleanup & simplify svg tree removing unecessary clips
    (tree-insert! maingroup-attrib 1 extra-latex-attrib)
    ;; for textext compatibility
    (tree-insert! maingroup-attrib 2 extra-tm-attrib)
    (move-node! defs maingroup 2)
    ;; move defs containing the glyph outlines inside main group
    ;; so that they remain together in inkscape

    ;; Fifth : finally create output
    (let* (;; convert back to stree, recreate the *TOP* node,
           ;; and restore *PI* xml
           ;; (instead of *PI* "xml" given by tree->stree -
           ;; otherwise serialize-html fails)
           (s-svg-out
            (append '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
                    ;; actually we use only ascii
                    (cddr (tree->stree buftree))))
           (xml-svg-out (begin (output-flush) ;; necessary??
                               (serialize-tmml s-svg-out))))
      ;; close temporary buffer
      (buffer-pretend-saved mybuf)
      (buffer-close mybuf)
      (string-save xml-svg-out dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (clipboard-copy-image void)
  (:synopsis "Places an image of the current selection on the clipboard")
  (:argument void "not used")
  (:returns "nothing")
  ;;the format of the graphics is set in the preferences
  (if (not (qt-gui?))
    (set-message "Qt GUI only, sorry. Use \"Export selection...\"" "")
    (if (not (selection-active-any?))
      (set-message "no selection!" "")
      (let* ((format (get-preference "texmacs->image:format"))
             (tmpurl (url-temp-ext format)))
        (export-selection-as-graphics tmpurl)
	;; first generate an image file
        (graphics-file-to-clipboard tmpurl)
	;; place that image on the clipboard
        (system-remove tmpurl)
	))))

(tm-define (export-selection-as-graphics myurl)
  (:synopsis "Generates graphics format of the current selection")
  (:argument myurl "A full file url with extension")
  (:returns "nothing")
  ;; global document parameters such as style, fonts, etc. are respected
  ;; in the typesetting. However they are presently not passed to
  ;; the svg and therefore lost when re-editing the svg

  (when (or (== (url-suffix myurl) "")
            (not (converter-search "pdf-file" (file-format myurl))))
    (with format (get-preference "texmacs->image:format")
      (when (not (converter-search "pdf-file" (string-append format "-file")))
        (set! format "pdf"))
      (with suffix (format-default-suffix format)
        (set! myurl (url-glue myurl (string-append "." suffix))))))

  (if (not (selection-active-any?))
      (set-message "no selection!" "")
      (let* (
        ;; step 1 prepare and typeset selection
        ;; if selection is part of math need to re-encapsulate
        ;; it with math to obtain proper typesetting :
        (tm-fragment
          (if (in-math?)
              (stree->tree `(equation* (document ,(selection-tree))))
  	          (selection-tree)))
        ;; also if selection spans several lines of text,
        ;; need to encapsulate it in a fixed-width table
        ;;to enforce pagewidth :
        (tm-fragment-enforce-pagewidth
          (if (tree-multi-paragraph? (selection-tree))
              (stree->tree
                `(tabular
                   (tformat (twith "table-width" "1par")
			               (twith "table-hmode" "exact")
			               (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                     (table (row (cell (document ,tm-fragment)))))))
              (selection-tree)))
        (temp0 (url-temp-ext "pdf"))
        (dpi-pref (get-preference "printer dpi"))
        (suffix (url-suffix myurl)))

       (set-printer-dpi "236") ; 472 is ~ exact size
       ;;set to a fixed value so our graphics does
       ;;not depend on the printer dpi
       ;;We need to set this weird dpi value so that the size of the svg
       ;;produced is about twice that of direct pdf or ps output. Why??
       (print-snippet temp0 tm-fragment-enforce-pagewidth #t)
       ;;typeset fragment to ps as starting point
       (set-printer-dpi dpi-pref)
       ;; revert to preference dpi
       ;; step 2 generate output according to desired output format

       (cond
        ((== suffix "pdf") (system-copy temp0 myurl))
        ((== suffix "svg")
         ;; assume target is inkscape
         (file-convert temp0 myurl)
         ;; using either pdf2svg or pdf2cairo converters is crucial
         ;; for svg inport in inkscape:
         ;; fonts are properly passed as vector outlines
         ;; file converters are defined in init-images.scm
         (refactor-svg myurl tm-fragment))
         ;; modify svg, embedding texmacs code
        (else
         (file-convert temp0 myurl)))

       (system-remove temp0) ;; temp pdf file not needed anymore
  )))
