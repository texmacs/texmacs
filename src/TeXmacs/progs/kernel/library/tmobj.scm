
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmobj.scm
;; DESCRIPTION : the hybrid tree or list-like data format
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; While low-level manipulations on TeXmacs trees are directly done on
;; the 'tree' type from TeXmacs, high-level manipulations like format
;; conversions are more conveniently done using classical S-expressions.
;; The tmobj class aims to unify both approaches. An instance of type tmobj
;; is either a string, a tree or a list (lab ch1 ... chn), where lab is
;; a symbol and ch1, ..., chn are children of type tmobj.
;;
;; *** We should wait for the implementation of internal drd's
;; *** Handling the 'expand' problem is still to complicated,
;; *** especially when dealing with paths.
;;
;; FIXME: better name for 'tmobj'. tmdata? abstree? ...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library tmobj)
  (:use (kernel texmacs tm-define))
  (:export
    tmobj-car tmobj-cdr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmobj-car t)
  (:type (tmobj -> symbol))
  (:synopsis "Get the label of @t.")
  (if (tree? t) (tree-get-label t) (car t)))

(tm-define (tmobj-cdr t)
  (:type (tmobj -> (list tmobj)))
  (:synopsis "Get the arguments of @t.")
  (if (tree? t) (tree-get-children t) (cdr t)))
