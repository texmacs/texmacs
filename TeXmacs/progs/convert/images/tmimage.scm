
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
        (convert tmml tmtmml)
        (ice-9 regex)))

;; (display "Texmacs] Loading module tmimage\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert active selection to various graphics format
;; try embedding texmacs code of the selection in metadata of the image for
;; re-edition.
;; the svg produced by this method can be pasted in inkscape via the clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; temporary files with extensions (for debugging)

(define (temp-url-ext ext)
  (url-unix "" (string-append (url->string (url-temp)) "." ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commodity functions for tree manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-node! node)
  ;; removes node (with children if any)
  (tree-remove! (tree-ref node :up) (tree-index node) 1))

(define (move-node! node parent-dest pos)
  ;; moves an existing node (with children if any) and
  ;; insert it as new child of parent-dest
  ;; FIXME: No sanity check! parent should not be in node's subtree! 
  (tree-insert! parent-dest pos `(,node))
  (remove-node! node))

(define (replace-leaftext! leaf newtext)
  ;; replace a node's content by a new string.
  ;; Makes the node a leaf if it wasn't one
  (tree-assign! leaf (string->tree newtext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2 functions for remapping cross-referenced items (glyphs, clip-box)
;; in the svg using unique ids this is needed to avoid collisions between
;; definitions belonging to differents formulas in inkscape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (newids! lablist seed-string) 
  ;; replaces all ids in the svg
  ;; plus returns an associationlist for mapping old id to new ones
  ;; we will use it to replace hyperlinks to the former ids to the new ones 
  (let* ((unique (number->string (random 1000000 (seed->random-state
                                                  seed-string))))
         ;; generate a 6-digit number that depends on the tm-code of
         ;; the selection should be reproducible for a given equation...
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

(define (adjust-clippath! maingr old-new-labels-alist)
  ;; again use the above association list to actualy replace
  ;; the clip-path href items with updated targets
  ;; alternatively the clip-path and first nested group
  ;; in main group could probably be removed altogether
  (let* ((clip (car (select maingr '(:* g @ clip-path :%1))))
         (cliptarget (match:substring (string-match "^url\\((.*)\\)$"
                                                    (tree->string clip)) 1))
         (newtarget (assoc-ref old-new-labels-alist cliptarget))
         (newurl (string-append "url(" newtarget ")"))
         ;; (newurl (format #f "url(~A)" newtarget))
         )
    (replace-leaftext! clip newurl)))

(define-preferences
  ("texmacs->graphics:tmml" "on" noop)
  ("texmacs->graphics:attr" "on" noop)
  ("texmacs->graphics:format" "svg" noop))

(define (refactor-svg dest tm-fragment)
  ;; reorganize svg file and inject attributes containing tm code of
  ;; equation dest is the url of the svg file to be edited
  ;; A latex fragment is also added for compatibility with
  ;; 'textext' inkscape extension
  ;; FIXME : no error checking, no return value... 
  ;; FIXME if the selection contains a postscript figure
  ;; it seems not escaped correctly.

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
       (idlist (select svgroot '(:* id :%1))) ; list of all ids in the drawing, used to label glyph outlines and clip-path definition
       (hreflist (select maingroup '(:* @ xlink:href :%1))) ; list of hyperlinks to the glyphs labels
       ;; define latex and texmacs string representation of selection
       ;; we escape them to ascii so that they do not interfere with xml
       ;; <  -> &lt;  > -> &gt; \ -> \\, all characters above #127->\xXX ... 
       ;; see  TeXmacs/langs/encodings/cork-escaped-to-ascii.scm
       ;; for the latex representation we mimick what is done when
       ;; "copy to latex" is performed 
       (latex-tree (latex-expand tm-fragment))
       ;; expand or not macros according to preferences
       (latex-code (texmacs->generic latex-tree "latex-snippet"))
       ;; actual conversion
       (latex-code (escape-to-ascii latex-code)) 
       (tm-code (serialize-texmacs tm-fragment))
       (tm-code (escape-to-ascii tm-code)) 
       ;; define new attributes containing latex and texmacs code:
       (extra-latex-attrib
        `((xmlns:ns0 "http://www.iki.fi/pav/software/textext/") 
          (ns0:text ,latex-code) (ns0:preamble "texmacs_latex.sty"))) 
       (extra-tm-attrib `((xmlns:ns1 "http://www.texmacs.org/") 
                          (ns1:texmacscode ,tm-code)))
       ;; OK, the texmacs namespace maybe not correctly described at that url
       ;; as an alternative to inserting the tm-code as attribute string,
       ;; we can embbed it as xml in the svg :
       (tmml-fragment
        `((desc (@ (id "texmacs"))
                (TeXmacs (@ (xmlns "http://www.texmacs.org/")
                            (version ,(texmacs-version-release "")))
                         (tm-par ,(tmtmml (tree->stree tm-fragment)))))))
       (old->new-labels (newids! idlist tm-code))
       ;; rename all ids, create an association list of old to new ids
       )
    ;;(display tm-code)
    
    ;; Third: modify tree
    (replace-hlinks! hreflist old->new-labels)
    ;; replace hlinks with new pointers
    (adjust-clippath! maingroup old->new-labels)
    ;; clip path uses another hlink syntax, needs to be treated separately
    (tree-insert! maingroup-attrib 1 extra-latex-attrib)
    ;; for textext compatibility
    (if (== "on" (get-preference "texmacs->graphics:attr"))
        (tree-insert! maingroup-attrib 2 extra-tm-attrib))
    (move-node! defs maingroup 3)
    ;; move defs containing the glyph outlines inside main group
    ;; so that they remain together in inkscape
    (if (== "on" (get-preference "texmacs->graphics:tmml"))
        (tree-insert! maingroup 4 tmml-fragment)) 
    ;; TODO/FIXME : choose what embedding format is preferred for
    ;; native tm data, it is pointless to have both.
    ;; presently 
    ;; - the re-edition mechanism favors the attribute if both
    ;;   are present (latex is always last choice).
    ;; - the attribute method works best because tmml format is
    ;;   *broken both on export and import* (and should be fixed IMHO).
    ;; But tmml seems more natural to include in svg, and gives more room
    ;; for improvements (we could pass the style that was used when
    ;; the equation was created, the fonts,...)
  
    ;; Fourth : finally create output
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
      ;; (display s-svg-out)
      (string-save xml-svg-out dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (export-selection-as-graphics output)
  (:synopsis "Generates graphics format of the current selection")
  (:argument output "A full file name (-> disk) or just an extension (-> clipboard)")
  (:returns "nothing")
  ;;output must either be a full file path or an extension type
  ;; FIXME : no error checking, no retun value... 
  ;; FIXME : external tools calls are not OS independent presently
  ;; no check is performed on presence of the required conversion tools
  ;; global document parameters such as style, fonts, etc. are respected
  ;; in the typesetting. However they are presently not passed to
  ;; the svg and therefore lost when re-editing the svg

  ;; (import-from (convert html htmlout))
  (if (selection-active-any?) 
      (let* (;;step 1 figure what is the wanted destination
             (dest (string->url output))
             (clipboard (and (== (url-head dest) (url-unix "" "."))
                             (== (url-suffix dest) "")))
             ;; if name contains no extension & no path,
             ;; then assume it's only an extension type and send to clipboard
             (myurl (if clipboard (temp-url-ext output) dest))
             (suffix (url-suffix myurl))
             ;; step 2 preprare and typeset selection
             ;;if selection is part of math need to re-encapsulate
             ;; it with math to obtain proper typesetting :
             (tm-fragment
              (if (tree-multi-paragraph? (selection-tree)) (selection-tree) 
                  (if (in-math?)
                      (stree->tree `(equation* (document ,(selection-tree))))
                      (selection-tree))))
             ;; also if selection spans several lines of text,
             ;; need to encapsulate it in a fixed-width table
             ;;to enforce pagewidth :
             (tm-fragment-enforce-pagewidth
              (stree->tree
               `(tabular
                 (tformat (twith "table-width" "1par") 
                          (twith "table-hmode" "exact") 
                          (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                          (table (row (cell (document ,tm-fragment))))))))
             (temp1 (temp-url-ext "eps")))
        ;; (display temp1)
        (print-snippet temp1 tm-fragment-enforce-pagewidth)
        ;; typeset fragment to eps as starting point

        ;; step 3 generate output according to desired output format
        (cond ((== suffix "eps")
               (system-copy temp1 myurl))
              ((== suffix "pdf")
               (system-2 "ps2pdf -dEPSCrop" temp1 myurl)
               (system-remove temp1))
              ((== suffix "svg")  
               ;; assume target is inkscape with texmacs.ink plugin allowing to
               ;; re-edit the original tm selection (presumably an equation) 
               (let* ((temp2 (temp-url-ext "pdf")))
                 ;; still need pdf as intermediate format                 
                 (system-2 "ps2pdf -dEPSCrop" temp1 temp2) 
                 (system-2 "pdf2svg" temp2 myurl) 
                 ;; chaining these 2 specific converters is crucial
                 ;; for svg inport in inkscape:
                 ;; fonts are properly passed as vector outlines
                 (refactor-svg myurl tm-fragment)
                 ;; modify svg, embedding texmacs code
                 ;; (system-remove temp2)
                 ))
              (else
                ;; other formats : use imagemagick generic converter
                ;; this is where png, jpg, etc is generated  
                ;; we ask imagemagick to insert texmacs source
                ;; in image metadata (comment)
                (system-2
                  (string-append "convert -comment \"" 
                                 (escape-to-ascii
                                  (serialize-texmacs tm-fragment)) "\"")
                  temp1 myurl)))
          
        (system-remove temp1) ; temp eps file not needed anymore

        (when clipboard
          (if (qt-gui?)
              (graphics-file-to-clipboard myurl)
              (set-message "Qt GUI only, sorry. Use \"Export selection...\"" ""))
          ;; (system-remove myurl)
          ) ;; for the clipboard destination (to be removed afterwards)
        ) ;; close let* for selection-any?==#t
      (set-message "no selection!" "")))
