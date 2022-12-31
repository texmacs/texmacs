;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmimage.scm
;; DESCRIPTION : convert texmacs fragment (selection) to image formats.
;; COPYRIGHT   : (C) 2012-2022  Philippe Joyez
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert active selection to various graphics format
;; If target is svg embed texmacs code of the selection in the image for
;; re-edition (Could be done for other formats too).
;; the svg produced by this method can be pasted in inkscape via the clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert images tmimage)
  (:use (convert tmml tmmlout)
        (convert tmml tmtmml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling of image convertion preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-image-export-format)
  (if (nnot (converter-search "pdf-file" "svg-file")) "svg" "pdf"))

(define-preferences
  ("texmacs->image:format" (default-image-export-format) noop))
  ;("texmacs->image:scale" "2.0" noop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (url-temp-ext ext)
;; temporary files with extensions
  (url-glue (url-temp) (string-append "." ext)))

(if (not (defined? 'string-contains)) ; for s7
    (define (string-contains ss s)
       (string-position s ss)))

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

(define (selection-trim-ending) ; code from text-edit.scm
              (if (selection-active-any?)
               (with st (selection-tree)
                  (if (and (not  (tree-atomic? st ))
                          (tree-empty? (tree-ref st :last)))
                   (begin
                     (selection-set
                        (selection-get-start)
                        (path-previous (root-tree) (selection-get-end)))
                     (selection-trim-ending))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functions needed for making reeditable svg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define latex and texmacs string representation of selection
;; we escape them to ascii so that they do not interfere with xml
;; <  -> &lt;  > -> &gt; \ -> \\, all characters above #127->\xXX ...
;; see  TeXmacs/langs/encodings/cork-escaped-to-ascii.scm
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

(define (refactor-svg dest tm-fragment relbaseline)
;; dest is the url of the svg file to be edited
;; We reorganize & cleanup the svg file and inject attributes containing:
;; - the tm code of equation
;; - style info from the original document (style, fonts, layout, ...)
;; - A latex fragment for compatibility with 'textext' inkscape extension
;; - the relative position of the baseline to enable vertical alignement
;;   in an external application
;; FIXME : no error checking, no return value...

  (let*
    ( ;; 1: load svg and transform to an active tree in
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

      ;; 2: define a bunch of locations in the tree
      (buftree (buffer-get-body mybuf)) ;; the whole tree
      (svgroot (car (select buftree '(:* svg)))) ;; the <svg > node
      (maingroup (car (select svgroot '(g))))
      ;; the main group in the svg, containing the drawing layout
      (maingroup-attrib (car (select maingroup '(@))))
      ;; attributes of the main group
      (defs (select svgroot '(defs)))
      (defs (if (list>0? defs) (car defs) #f))
      ;; the defs, containing the glyph vector outlines,
      ;; hyperlinked from the drawing (a.k.a cloned)
      (bgframe (select maingroup '(:* path @ style :%1)))
      (bgframe (if (null? bgframe) #f (car bgframe)))
      ;; the solid background we introduced
      ;; 3: the new data we want to insert in the tree
      (latex-code (latex-encode tm-fragment))
      (tm-code (tm-encode tm-fragment))
      (tm-inits (tm-encode (get-all-inits)))
      (tm-style (tm-encode (get-style-tree))) 
      ;;; define new attributes containing latex and texmacs code:
      (extra-latex-attrib
      `((xmlns:ns0 "http://www.iki.fi/pav/software/textext/")
        (ns0:text ,latex-code) (ns0:preamble "texmacs_latex.sty")))
      (extra-tm-attrib
      `((xmlns:ns1 "https://www.texmacs.org/")
        (ns1:texmacscode ,tm-code) (ns1:texmacsstyle ,tm-inits) (ns1:texmacsstyle2 ,tm-style)
        (ns1:texmacsbaseline ,relbaseline)))
      ;; OK, the texmacs namespace maybe not correctly described at that url ...
    )

    ;; 4: modify tree
    ;; 4.1 set the background fully transparent
    (if (and bgframe (== (tree->string bgframe) " stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:0.007;"))
        (tree-set! bgframe " stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:0.00"))
    ;; 4.2 move defs containing the glyph outlines inside main group
    ;; so that they remain together in inkscape
    (if defs (move-node! defs maingroup 2))
    ;; 4.3 (not optional!), add our own new attributes for re-editting equation
    (tree-insert! maingroup-attrib 1 extra-latex-attrib) ;; for textext compatibility
    (tree-insert! maingroup-attrib 2 extra-tm-attrib)
    
    ;; 5: finally create output
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
      (string-save xml-svg-out dest)
      ;(if (url-exists-in-path? "scour") ; scour can simplify svg  https://github.com/scour-project/scour
      ;    (with tu (url-temp) 
      ;      (system-2 "scour" (url-sys-concretize dest) (url-sys-concretize tu))
      ;      (system-move tu dest)))
            
            )))

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
  ;; for svg export, the texmacs code of the selection as well as
  ;; global document parameters such as style, fonts, etc. are
  ;; embedded in the drawing to enable re-editing from inkscape
  
  ;; we try reasonably hard to have the wysiwyg behavior:
  ;; the image should ideally reproduce what is on the screen, including line breaks if any 
 (if (not (selection-active-any?)) (set-message "no selection!" "")
   (begin 
    (if (== (url-suffix myurl) "")
      (with format (get-preference "texmacs->image:format")
        (show-message (string-append "No file extension specified, defaulting to " format) "No image format given" )
        (with suffix (format-default-suffix format)
          (set! myurl (url-glue myurl (string-append "." suffix))))))
  
    (with suffix (url-suffix myurl)
      (when (not (file-converter-exists? "x.pdf" (string-append "y." suffix)))
        (show-message (string-append "Sorry, pdf to " suffix " converter is missing. Generating pdf instead") "Image format not available" )
        (let* ((sufl (string-length suffix))
              (surl (url->string myurl))
              (sl (string-length surl)))
          (set! myurl (string->url (string-append (substring surl (- sl sufl) sl) "pdf"))))))

; TODO Handle when output file already exists (presently we overwritte without warning)

    (selection-trim-ending)
    (let*
     ((suffix (url-suffix myurl))
;; step 1 prepare and typeset selection
;; if selection is inside inline or display math preserve inline/display style
      (issomemath (nnot (match? (tree->stree (selection-tree) ) 
                    '(:or (equation* :*) (equation :*) (eqnarray :*) (eqnarray* :*) (math :*) (align :*) (align* :*))) ))
      (inmath (== (tree->string (get-env-tree-at "mode" (selection-get-start))) "math"))
      (indisplaymath (==  (tree->string (get-env-tree-at "math-display" (selection-get-start))) "true"))
     
      (tm-fragment
        (cond
          (issomemath  (display "selection tree is a math tag \n")
                       (selection-tree)) 
          (inmath
             (if indisplaymath
               (begin (display "selection tree is in display math \n" )
                 (stree->tree `(equation* ,(selection-tree))))
               (begin (display "selection tree is in inline math \n" )
                 (stree->tree `(math ,(selection-tree))))))
          (else (display "selection not purely math \n") (selection-tree))))
;; is selection wider than 1par (and needs linebreaks and or hyphenation)?
      (maxwidth (length-decode "1par"))
      (partmpt (string-append (number->string maxwidth) "tmpt"))
      (parcm (length-add  "0cm" partmpt))
      ;; get-page-width return dimension in the cpp "SI" unit which seems to be == "10tmpt"
;; output scale of image?
      ;(str-scale  (get-preference "texmacs->image:scale"))
      ;(scale (string->number str-scale))

;; We compute the baseline position only if it's a single-line content
;; (this excludes selections begining with 'document)
;; If the selection is eqnarray or similar compute it only if the table
;; has a single row
      (iseqnarray (nnot (match? (tree->stree tm-fragment) 
                    '(:or (eqnarray :*) (eqnarray* :*) (align :*) (align* :*)))))
      (table-t (if iseqnarray (tree-ref (selection-tree) :* 'table) #f))
      (eqarraynrows (if table-t (length (select table-t '(:%0 row ))) #f ))
      (simpleeqnarray (and  iseqnarray (== 1 eqarraynrows)))
      (tmppng (url-temp-ext "png") )
      (extents (print-snippet tmppng (selection-tree) #t ) )
      (rawwidth (- (third extents) (first extents))) 
      ;NOTE: we use print-snippet because (box-info (selection-tree) "W")
      ; does not yield a reliable measure. why?
      (rawwidthOK (< rawwidth maxwidth))
      (needbaseline 
        (if (match? (tree->stree tm-fragment) '(document :*)) 
          #f   
         (if iseqnarray 
           (if (and simpleeqnarray rawwidthOK) #t #f)
           rawwidthOK)))

;; the baseline calculation is relative to the size of the background frame
;; poppler puts a background frame in the svg image only if not fully transparent (otherwise no)
;; (during svg postprocessing  we'll set the opacity of the background to 0)  
      (fillcolor (if (and needbaseline (== suffix "svg")) "#ffffff02" "#ffffff00")) ; either slightly opaque or fully transparent white

;; if selection is an equation array, make table width minimal to avoid wide white frame
      (tm-fragment1 
        (if iseqnarray 
          (with tfmt (tree-ref tm-fragment :* 'tformat)
             (tree-insert tfmt 0 '((twith "table-hmode" "min")))
             tm-fragment)
          tm-fragment))
;because of bug #63404 we can't simply always use document-at for formating 
      (tm-fragment-formated
        (if needbaseline
          ;; if needbaseline insert fragment in table having a background

          `(with 
            ;"page-width" ,pagewcm
            (tabular (tformat
                (twith "table-width" ,parcm)
                (twith "table-hmode" "min")
                (twith "table-valign" "B") ;;=baseline of top line
                (cwith "1" "1" "1" "1" "cell-background" ,fillcolor)
                ;(cwith "1" "1" "1" "1" "cell-hyphen" "t") ; see bug #63404
                (cwith "1" "1" "1" "1" "cell-lsep" "0spc")
                (cwith "1" "1" "1" "1" "cell-tsep" "0sep")
                (cwith "1" "1" "1" "1" "cell-rsep" "0spc")
                (cwith "1" "1" "1" "1" "cell-bsep" "0sep")
                (table (row (cell ,tm-fragment1)))))
            )
          ;; otherwise (multiline selection) use doc-at to get proper pagewidth
          `(with 
            ;"page-width" ,pagewcm
                "fill-color" ,fillcolor
                "doc-at-width" ,parcm ;
                "doc-at-hmode" ,(if (or iseqnarray indisplaymath inmath)  "min" "exact") ;
                ;"doc-at-valign" "base"
                "doc-at-padding" "0spc"
                (document-at (document ,tm-fragment1) (point "0par" "0")))
        ))

;; step 2 generate output according to desired output format

    (extents (print-snippet myurl (stree->tree tm-fragment-formated) #t)); scale))
;; compute relative position of baseline from returned box dimensions  see tmhtml.scm
    (height (- (fourth extents) (second extents)))
    (relbaseline (if needbaseline (number->string (exact->inexact (/ (- (sixth extents)) height))) "0.0"))
           ); end of let* defs

      (system-remove tmppng)
      (if (== suffix "svg")
        (begin 
         (display* "relbaseline= " relbaseline "\n")
         (refactor-svg myurl tm-fragment relbaseline))
         ;; modify svg, embedding texmacs code
        )

    ))))
