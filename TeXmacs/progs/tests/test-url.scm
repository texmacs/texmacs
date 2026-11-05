;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-url.scm
;; DESCRIPTION : TeXmacs URL library unit tests
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-url)
  (:use (tests test-runner)))

(define-test "url: conversion between string and url"
  (let* ((str "doc.tm")
         (u (string->url str)))
    (assert-true (url? u))
    (assert-false (url-none? u))
    (assert-equal str (url->string u))))

(define-test "url: head and tail manipulation"
  (let ((u (string->url "dir/file.txt")))
    (assert-equal "dir" (url->string (url-head u)))
    (assert-equal "file.txt" (url->string (url-tail u)))))

(define-test "url: append and concatenation"
  (let* ((u1 (string->url "parent"))
         (u2 (string->url "child.tm"))
         (joined (url-append u1 u2)))
    (assert-true (url? joined))
    (assert-true (or (string= (url->string joined) "parent/child.tm")
                     (string= (url->string joined) "parent\\child.tm")))))

(define-test "url: predicates"
  (assert-true (url-none? (url-none)))
  (assert-false (url-none? (string->url "valid.tm")))
  (assert-false (url-rooted? (string->url "relative/path"))))
