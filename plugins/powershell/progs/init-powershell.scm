;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-powershell.scm
;; DESCRIPTION : Initialize PowerShell plugin
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (powershell-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append s "\n<EOF>\n"))))

(define (powershell-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/powershell/bin/tm_powershell.ps1")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/powershell/bin/tm_powershell.ps1")
      (system-url->string "$TEXMACS_PATH/plugins/powershell/bin/tm_powershell.ps1")))

(define (powershell-launcher)
  (string-append
   (if (url-exists-in-path? "powershell") "powershell.exe" "pwsh.exe")
  " -NoLogo -NoProfile -ExecutionPolicy Bypass -File \""
   (powershell-entry) "\""))

(plugin-configure powershell
  (:require (or (url-exists-in-path? "powershell")
                (url-exists-in-path? "pwsh")))
  (:launch ,(powershell-launcher))
  (:serializer ,powershell-serialize)
  (:session "PowerShell"))