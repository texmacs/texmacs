# Server Integration Tests

Integration tests for the TeXmacs client-server protocol.
Each test launches a real TeXmacs client process that connects over TLS to a
live server, executes Scheme scenario scripts, and checks the outcome.

## Directory layout

```
misc/server-test/
  main_test.go                  Go test runner (TestLoad, TestFeatures)
  go.mod / go.sum               Go module (only testify as dependency)
  compose.yaml                  Docker Compose: texmacs-server + mailpit
  setup-test-server.sh          Legacy: start server container manually
  scripts/setup-test-server.sh  Extended server setup script
  preferences.scm               Server preferences mounted into container
  mail.rc                       Mail config mounted into container
  my-init-texmacs.scm           Generated — cat of fixture/helpers/*.scm

  fixture/helpers/
    helpers.scm                 Server-side fixture API (fixture-create-account,
                                fixture-create-file, fixture-create-chat,
                                fixture-share, etc.)
    generate-cert.scm           Self-signed TLS certificate generation
    admin.scm                   Admin account creation
    client-helpers.scm          Client-side test helpers (fail!, check!,
                                setup-test, with-server-eval)

  scenarios/
    load/                       Stress scenarios — fire RPCs, no assertions
      scenario-login-logout.tmpl.scm
      scenario-get-accounts.tmpl.scm
      scenario-get-shared-resources.tmpl.scm
      scenario-print-public-preferences.tmpl.scm
      config.tmpl.scm
    features/                   Feature scenarios — assert correct behaviour
      scenario-delete-account.tmpl.scm
```

## How it works

### Server

The server runs in Docker (`compose.yaml`).  `my-init-texmacs.scm` is mounted
as the server's init script — it defines fixture helper functions and creates
the admin account.  These helpers remain available in the server's Guile
namespace and can be called from scenario scripts via `remote-eval`.

### Test runner (main_test.go)

Two Go test functions, sharing a common `runScenarios` helper:

- **`TestLoad`** — parses `scenarios/load/*.tmpl.scm`, renders each through
  Go's `text/template` (injecting `{{.Host}}`, `{{.Port}}`, `{{.Seed}}`,
  admin credentials), and executes the resulting Scheme script in a headless
  TeXmacs client process.  No client-side assertion helpers are prepended.

- **`TestFeatures`** — same flow but for `scenarios/features/*.tmpl.scm`.
  The content of `fixture/helpers/client-helpers.scm` is prepended to every
  script, providing `fail!`, `check!`, `setup-test`, and `with-server-eval`.

Both test functions:
- Run each scenario in parallel via `t.Parallel()`
- Inject a random seed (`TEST_SEED` env or crypto/rand) so concurrent runs
  create non-colliding fixture data
- Write the rendered scripts to `debug/` for post-mortem inspection
- Fail the Go test if the TeXmacs process exits non-zero

### Scenario templates

Templates are Go `text/template` files producing Scheme code.
Available template variables:

| Variable                  | Example                |
|---------------------------|------------------------|
| `{{.Host}}`               | `localhost`            |
| `{{.Port}}`               | `6561`                 |
| `{{.Seed}}`               | `8429371650123`        |
| `{{$a := index .Accounts "admin"}}` | admin account |
| `{{$a.Username}}`         | `admin`                |
| `{{$a.Password}}`         | env `ADMIN_PASSWORD`   |

### Load scenarios

Fire-and-forget RPC actions against the server — login, fetch accounts, list
shared resources, etc.  No assertions; the test passes if the script exits
cleanly.  Designed to be run with high concurrency (`-count=N`, `-parallel=M`)
for stress testing.

### Feature scenarios

Self-contained tests that:
1. **Setup** — create fixtures server-side via `setup-test` (fire-and-forget,
   forms are quoted verbatim and sent through `remote-eval`)
2. **Query** — read back state via `with-server-eval` (binds result into
   client-side variables, supports destructuring)
3. **Assert** — `check!` verifies conditions, `fail!` aborts with exit code 1
4. **Cleanup** — delete test accounts via `remote-delete-account`

Seed-based unique names (`"alice-del-{{.Seed}}"`) ensure parallel feature tests
don't collide.

## Fixture helpers

### Server-side (`helpers.scm`, loaded at server startup)

| Function                                           | Purpose                                   |
|----------------------------------------------------|-------------------------------------------|
| `(fixture-create-account pseudo name pw email adm?)` | Create a user with salted TLS credentials |
| `(fixture-create-file pseudo filename perms)`      | Create a remote file resource             |
| `(fixture-create-dir pseudo dirname perms)`        | Create a remote directory resource        |
| `(fixture-create-chat pseudo room-name perms)`     | Create a chat room                        |
| `(fixture-create-live pseudo name perms)`          | Create a live collaborative document      |
| `(fixture-share from-pseudo to-pseudo rid server)` | Post a share notification to a user's mailbox |

`perms` format: `'(("pseudo" readable? writable? owner?) ...)`

### Client-side (`client-helpers.scm`, prepended to feature scenarios)

| Form                                        | Purpose                                        |
|---------------------------------------------|-------------------------------------------------|
| `(fail! label msg)`                         | Print FAIL and exit 1                           |
| `(check! label ok? msg)`                    | Assert; calls `fail!` when `ok?` is `#f`       |
| `(on-error label)`                          | Returns an error callback that calls `fail!`    |
| `(setup-test server form ...)`              | Macro — quote forms as `(begin ...)`, send via `remote-eval`, fire-and-forget |
| `(with-server-eval r server expr body ...)` | Macro — eval `expr` server-side, bind result to `r` (supports destructuring via `with`), run `body` |

## Environment variables

| Variable              | Required | Default     | Purpose                                 |
|-----------------------|----------|-------------|-----------------------------------------|
| `TEXMACS_PATH`        | yes      |             | Path to TeXmacs installation            |
| `TEXMACS_SERVER_CERT` | yes      |             | Path to server's TLS certificate (PEM)  |
| `ADMIN_PASSWORD`      | yes      |             | Admin account password                  |
| `TEXMACS_HOME_PATH`   | no       | temp dir    | Client home directory                   |
| `TMSERVER_HOST`       | no       | `localhost` | Server hostname                         |
| `TMSERVER_PORT`       | no       | `6561`      | Server port                             |
| `TMSERVER_PROTOCOL`   | no       | `tls`       | Protocol (`tls` or `legacy`)            |
| `TEST_SEED`           | no       | random      | Deterministic seed for reproducibility  |

## Running

```bash
# Start the server
docker compose up -d

# Run all tests
make run-tests

# Run only feature tests
go test -v -run TestFeatures

# Run load tests with higher concurrency
go test -v -run TestLoad -count=10

# Reproduce a specific run
TEST_SEED=8429371650123 go test -v -run TestFeatures
```
