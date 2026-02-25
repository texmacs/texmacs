(use-modules (client client-sync))

(define imgdir (system->url "/home/robin/Documents/texmacs-docs/images"))
(define image-labels "abcdefghijklmnopqrstuvwxyz")
(define file-pass "TeXmacs123!")
(define rand-state (seed->random-state test-seed))
(define users
  (map (lambda (i)
         (with user (string-append "account-instance-"
                                   test-host "-"
                                   test-port "-"
                                   "client-" test-client-nb "-"
                                   (number->string i) "-"
                                   test-seed)
           `(,user ,(string-append user "@localhost"))))
       (.. 0 2)))

(define (random-image imgdir)
  (with c (string-ref image-labels
                      (random (string-length image-labels) rand-state))
    (url-append imgdir (string-append "label_" (char->string c) ".png"))))

(define (image->stree file w h x y)
  (let* ((data (string-load file))
         (name (url->string (url-tail file))))
    `(image (tuple (raw-data ,data) ,name) ,w ,h ,x ,y)))

(define (load-image imgdir w h)
  (image->stree (random-image imgdir) w h "0" "0"))

(define (generate-doc imgdir n w h)
  (with imgs (map (lambda (n) (load-image imgdir w h)) (.. 0 n))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic"))
       (body (document ,@imgs)))))

(define (step-create-files user n images-per-file)
  (let* ((file-contents
           (map (lambda (i)
                  (generate-doc imgdir images-per-file "200" "200"))
                (.. 0 n)))
         (file-paths
           (map
             (lambda (data i)
               (url-unix "$TEXMACS_HOME_PATH"
                         (string-append user
                                        "-imagecache-testfile-"
                                        (number->string i)
                                        ".tm")))
             file-contents
             (.. 0 n))))
    (for-each (lambda (data fpath)
                (string-save (convert data "texmacs-stree" "texmacs-document") fpath))
      file-contents file-paths)
    file-paths))

(define (get-file-remote-link srv u)
  (url-append (remote-home-directory srv) (url-tail u)))

(define (step-load-files srv files)
  (delayed
    (:while #t)
    (:every 10000)
    (for (name files)
         (display* "got file name: " name "\n")
         (with rname (remote-file-name (get-file-remote-link srv name))
           (client-remote-eval srv `(remote-file-load ,rname)
             (lambda (tm)
               (with t (convert tm "texmacs-document" "texmacs-tree")
                 (fetch-missing-cache-refs srv t)))
             (lambda (err)
               (display* "error loading file: " err "\n")))))))

(define (run-on-instance asrv host port user)
  (display* "running on instance " host " " port ", user " user " \n")
  (with (pseudo email) user
    (with-server-eval
      _
      asrv
      `(fixture-create-account
         ,pseudo
         ,(string-append "File Tester " pseudo)
         ,file-pass
         ,email
         #f)
      (client-login-then
        host port
        pseudo `(tls-password ,file-pass)
        (lambda (srv ret)
          (if (!= ret "ready")
            (fail! "user-login" ret)
            (begin
              (add-active-connection srv host port pseudo)
              ;; Announce the client protocol version so the server enables
              ;; the tree cache (server-handle-cache gates tree-cache-update
              ;; on client-version>=? uid 1).
              (client-protocol-version-then srv
                (lambda (vret)
                  (let ((files (step-create-files pseudo 10 15)))
                    (display* "files created: " files "\n")
                    (for (file files)
                         (with dst (get-file-remote-link srv file)
                           (display* "uploading " file " -> " dst "\n")
                           (remote-upload
                             file dst "uploaded"
                             (lambda x
                               (display* "upload completed: "
                                         file " -> " dst "\n")))))
                    (step-load-files srv files)))))))))))

;; Entry point
(client-login-then
  test-host test-port test-admin-user `(tls-password ,test-admin-pass)
  (lambda (asrv ret)
    (if (!= ret "ready")
      (fail! "admin-login" ret)
      (for (user users)
           (run-on-instance asrv test-host test-port user)))))

