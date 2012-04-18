
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-tmfs.scm
;; DESCRIPTION : support for external versioning tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-tmfs))

(define version-tool-table (make-ahash-table))
(define version-tool-loaded (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch to support for various external tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (svn-active? name)
  (url-directory? (url-append (url-head name) ".svn")))

(tm-define (version-tool name)
  (if (ahash-ref version-tool-table name)
      (with tool (ahash-ref version-tool-table name)
        (and (!= tool "") tool))
      (with tool
          (cond ((svn-active? name) "svn")
                (else ""))
        (ahash-set! version-tool-table name tool)
        (when (and tool (not (ahash-ref version-tool-loaded tool)))
          (ahash-set! version-tool-loaded tool #t)
          (cond ((== tool "svn")
                 (module-provide '(version version-svn)))))
        (and (!= tool "") tool))))

(tm-define (versioned? name)
  (nnot (version-tool name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-history name) #f)

(tm-define (url-history name)
  (with s (url->tmfs-string name)
    (string-append "tmfs://history/" s)))

(tmfs-load-handler (history name)
  (with u (tmfs-string->url name)
    (with h (version-history u)
      ($generic
        ($tmfs-title "History of "
                     ($link (url->string u)
                       ($verbatim (url->string (url-tail u)))))
        ($when (not h)
          "This file is not under version control.")
        ($when h
          ($description-long
            ($for (x h)
              ($with (rel by date msg) x
                ($with dest (string-append "tmfs://release/" rel "/" name)
                  ($describe-item
                      ($inline "Version " ($link dest rel)
                               " by " by " on " date)
                    msg))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing a particular release
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-format-handler (release name)
  (with u (tmfs-string->url (tmfs-cdr name))
    (url-format u)))

(tm-define (version-release name rel) "")

(tmfs-load-handler (release name)
  (let* ((rel (tmfs-car name))
         (u (tmfs-string->url (tmfs-cdr name))))
    (version-release u rel)))
