;; admin account — fixed across all test runs
(fixture-create-account "admin" "Admin"
  (or (getenv "ADMIN_PASSWORD") "TeXmacs123!")
  "admin@localhost" #t)
