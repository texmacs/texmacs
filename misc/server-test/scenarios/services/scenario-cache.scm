;; Scenario: tree cache janitor / eviction paths against a real filesystem.
;;
;; disk_lru picks its victim by st_atime (tree_cache.cpp, scan_oldest),
;; some problems:
;;   - second resolution for stat on windows
;;   - default relatime mount will not show up changes if last accessed <24h
;;
(define cache-host
  (string-append "lru-test-" test-host "-" test-port
                 "-client-" test-client-nb "-" test-seed))

;; mirrors the tree_cache.hpp default (500 MB)
(define cache-default-max-size (* 500 1024 1024))

;; common let* bindings
(define (common-bindings)
  `((host ,cache-host)
    (root (string-append
            (url-concretize "$TEXMACS_HOME_PATH/system/tmp/tree_cache")
            "/" host))
    (shard-dir
      (lambda (h)
        (string-append root "/" (substring h 0 1) "/" (substring h 1 2))))
    (entry-path
      (lambda (h)
        (string-append (shard-dir h) "/" (substring h 2 (string-length h)))))
    (stats-path (lambda (h) (string-append (shard-dir h) "/stats")))
    ;; existence only -- must not open the entry, that would move atime
    (alive? (lambda (h) (url-exists? (system->url (entry-path h)))))
    (mk (lambda (payload)
          (stree->tree
            (list 'image
                  (list 'tuple (list 'raw-data payload) "image/png")
                  "100px" "100px" "" ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; single: the most recently read entry survives eviction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (evict-lru-expr)
  `(let* (,@(common-bindings)
          ;; equal-length, escape-free payloads -> identical serialized size,
          ;; so the janitor's choice is decided by atime and nothing else
          (ta (mk "AAAAAAAA"))
          (tb (mk "BBBBBBBB"))
          (ha (tree-hash ta))
          (hb (tree-hash tb))
          (_clear (tree-cache-clear host))
          (_put-a (tree-cache-update host ta))
          (_put-b (tree-cache-update host tb))
          ;; read A last so it becomes the most recently used entry and B the
          ;; eviction victim. 2s clears the whole-second atime resolution
          ;; regardless of when the second ticks.
          (_wait (var-eval-system "sleep 2"))
          (_get-a (tree-cache-get host ha))
          (total (tree-cache-size host))
          ;; leave room for exactly one of the two equal-sized entries
          (_cap (tree-cache-set-max-size host (+ (quotient total 2) 1)))
          (_run (tree-cache-janitor host))
          (a-survived (alive? ha))
          (b-survived (alive? hb))
          (_restore (tree-cache-set-max-size host ,cache-default-max-size))
          (_cleanup (tree-cache-clear host)))
     (list total a-survived b-survived)))

(define (evict-lru srv k)
  (with-server-eval result srv (evict-lru-expr)
    (with (total a-survived b-survived) result
      (display* "  [single] size=" total " survivors: A=" a-survived
                " B=" b-survived "\n")
      (check! "entries-were-cached" (> total 0)
        "cache reported zero bytes; the entries were never written")
      (check! "lru-keeps-recently-used" (and a-survived (not b-survived))
        (string-append "expected the read entry A to survive and B to be "
                       "evicted, got A=" (object->string a-survived)
                       " B=" (object->string b-survived)))
      (k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi: the eviction loop removes every older entry, not just one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (evict-many-expr)
  `(let* (,@(common-bindings)
          (t1 (mk "AAAAAAAA")) (t2 (mk "BBBBBBBB"))
          (t3 (mk "CCCCCCCC")) (t4 (mk "DDDDDDDD"))
          (h1 (tree-hash t1)) (h2 (tree-hash t2))
          (h3 (tree-hash t3)) (h4 (tree-hash t4))
          (_clear (tree-cache-clear host))
          (_p1 (tree-cache-update host t1))
          (_p2 (tree-cache-update host t2))
          (_p3 (tree-cache-update host t3))
          (_p4 (tree-cache-update host t4))
          ;; update atimes
          (_old (begin (tree-cache-get host h1) (tree-cache-get host h2)))
          (_wait (var-eval-system "sleep 2"))
          (_new (begin (tree-cache-get host h3) (tree-cache-get host h4)))
          (total (tree-cache-size host))
          (_cap (tree-cache-set-max-size host (+ (* 2 (quotient total 4)) 1)))
          (_run (tree-cache-janitor host))
          (s1 (alive? h1)) (s2 (alive? h2))
          (s3 (alive? h3)) (s4 (alive? h4))
          (_restore (tree-cache-set-max-size host ,cache-default-max-size))
          (_cleanup (tree-cache-clear host)))
     (list total s1 s2 s3 s4)))

(define (evict-many srv k)
  (with-server-eval result srv (evict-many-expr)
    (with (total s1 s2 s3 s4) result
      (display* "  [multi] size=" total " survivors: "
                s1 " " s2 " " s3 " " s4 " (old->new)\n")
      (check! "multi-entries-cached" (> total 0)
        "cache reported zero bytes; the entries were never written")
      (check! "evicts-oldest-keeps-newest" (and (not s1) (not s2) s3 s4)
        (string-append "expected the two older entries evicted and the two "
                       "newer kept, got h1=" (object->string s1)
                       " h2=" (object->string s2) " h3=" (object->string s3)
                       " h4=" (object->string s4)))
      (k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recount: the janitor reconciles drifted stats and skips leftovers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (recount-expr)
  `(let* (,@(common-bindings)
          (ta (mk "AAAAAAAA"))
          (tb (mk "BBBBBBBB"))
          (ha (tree-hash ta))
          (hb (tree-hash tb))
          (_clear (tree-cache-clear host))
          (_put-a (tree-cache-update host ta))
          (_put-b (tree-cache-update host tb))
          (real (tree-cache-size host))
          ;; simulate stats differ from a concurrent writer: Write the file directly
          (_inflate (string-save "999999999" (system->url (stats-path ha))))
          (_leftover (string-save "GARBAGE"
                       (system->url (string-append (shard-dir ha)
                                                   "/.tmp-crash-leftover"))))
          (inflated (tree-cache-size host))
          ;; a cap between real and inflated: should_evict fires on the inflated
          ;; total, but recount corrects it below the cap so nothing is evicted
          (_cap (tree-cache-set-max-size host (+ real 1000)))
          (_run (tree-cache-janitor host))
          (fixed (tree-cache-size host))
          (a-survived (alive? ha))
          (b-survived (alive? hb))
          (_restore (tree-cache-set-max-size host ,cache-default-max-size))
          (_cleanup (tree-cache-clear host)))
     (list real inflated fixed a-survived b-survived)))

(define (recount srv k)
  (with-server-eval result srv (recount-expr)
    (with (real inflated fixed a-survived b-survived) result
      (display* "  [recount] real=" real " inflated=" inflated
                " fixed=" fixed " survivors: A=" a-survived
                " B=" b-survived "\n")
      (check! "stats-was-forged" (> inflated real)
        "test setup failed: forging the stats file did not raise the size")
      (check! "recount-restores-true-size" (== fixed real)
        (string-append "recount did not restore the true size: expected "
                       (number->string real) " got " (number->string fixed)))
      (check! "no-eviction-under-cap" (and a-survived b-survived)
        (string-append "recount brought size under cap yet janitor evicted: "
                       "A=" (object->string a-survived)
                       " B=" (object->string b-survived)))
      (k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-probes srv)
  (evict-lru srv
    (lambda ()
      (evict-many srv
        (lambda ()
          (recount srv
            (lambda ()
              (display* "scenario-cache DONE.\n")
              (quit-TeXmacs-code 0))))))))

(client-login-then test-host test-port
  test-admin-user `(tls-password ,test-admin-pass)
  (lambda (asrv ret)
    (if (!= ret "ready")
      (fail! "admin-login" ret)
      (run-probes asrv))))
