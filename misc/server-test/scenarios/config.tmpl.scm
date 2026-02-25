{{ $adminAccount := index .Accounts "admin" -}}
(define test-host "{{.Host}}")
(define test-port "{{.Port}}")
(define test-protocol "{{.Protocol}}")
(define test-admin-user "{{$adminAccount.Username}}")
(define test-admin-pass "{{$adminAccount.Password}}")
(define test-seed "{{.Seed}}")
