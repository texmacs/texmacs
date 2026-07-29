{{ $adminAccount := index .Accounts "admin" -}}
(define test-instances `({{ range $index, $value := .Instances }} ("{{$value.Host}}" "{{$value.Port}}" {{ if $value.Protocol }} {{ $value.Protocol }} {{ end }}) {{ end }}))
(define test-instance  (car test-instances))
(define test-instance  (car test-instances))
(define test-client-nb "{{.Client}}")
(define test-host (car test-instance))
(define test-port (cadr test-instance))
(define test-protocol (if (> (length test-instance) 2) (caddr test-instance) "tls"))
(define test-admin-user "{{$adminAccount.Username}}")
(define test-admin-pass "{{$adminAccount.Password}}")
(define test-seed "{{.Seed}}")

(when {{if eq .Timeout 0}} #f {{else}} #t {{end}}
  (delayed (:pause {{.Timeout}}) (quit-TeXmacs-code 1)))
