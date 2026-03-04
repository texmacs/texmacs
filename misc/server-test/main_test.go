package servertest

import (
	"bytes"
	"crypto/rand"
	"encoding/binary"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"text/template"

	"github.com/stretchr/testify/require"
)

type Account struct {
	Username string
	Password string
}

type Environment struct {
	Host       string
	Port       string
	Protocol   string
	TMPath     string
	TMHomePath string
	IsTmpHome  bool
	TMExec     string
	Accounts   map[string]*Account
	Seed       uint64
}

func setupEnv() (*Environment, error) {
	tmPath := os.Getenv("TEXMACS_PATH")
	if tmPath == "" {
		return nil, errors.New("TEXMACS_PATH environment not set")
	}

	tmServerCert := os.Getenv("TEXMACS_SERVER_CERT")
	if tmServerCert == "" {
		tmServerCert = "certs/cert.pem"
	}

	certBytes, err := os.ReadFile(tmServerCert)
	if err != nil {
		return nil, fmt.Errorf("reading server cert %s: %w (is the server running?)", tmServerCert, err)
	}

	tmHomePath := os.Getenv("TEXMACS_HOME_PATH")
	isTmpHome := false
	if tmHomePath == "" {
		var err error
		isTmpHome = true
		tmHomePath, err = os.MkdirTemp("", "texmacs-test")
		if err != nil {
			return nil, err
		}
	}

	err = os.MkdirAll(filepath.Join(tmHomePath, "system/certificates"), 0o755)
	if err != nil {
		return nil, err
	}

	err = os.WriteFile(filepath.Join(tmHomePath, "system/certificates/trusted-certificates.crt"), certBytes, 0o644)
	if err != nil {
		return nil, err
	}

	tmServerHost := os.Getenv("TMSERVER_HOST")
	if tmServerHost == "" {
		tmServerHost = "localhost"
	}
	tmServerPort := os.Getenv("TMSERVER_PORT")
	if tmServerPort == "" {
		tmServerPort = "6561"
	}
	tmServerProto := os.Getenv("TMSERVER_PROTOCOL")
	if tmServerProto == "" {
		tmServerProto = "tls"
	}

	var seed uint64
	if s := os.Getenv("TEST_SEED"); s != "" {
		parsed, err := strconv.ParseUint(s, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid TEST_SEED: %w", err)
		}
		seed = parsed
	} else {
		var b [8]byte
		_, _ = rand.Read(b[:])
		seed = binary.LittleEndian.Uint64(b[:])
	}

	return &Environment{
		Host:       tmServerHost,
		Port:       tmServerPort,
		Protocol:   tmServerProto,
		TMPath:     tmPath,
		TMHomePath: tmHomePath,
		IsTmpHome:  isTmpHome,
		TMExec:     path.Join(tmPath, "bin/texmacs"),
		Accounts:   make(map[string]*Account),
		Seed:       seed,
	}, nil
}

func getAdminAccount() (*Account, error) {
	adminPwd := os.Getenv("ADMIN_PASSWORD")
	if adminPwd == "" {
		return nil, errors.New("no admin password in environment, set ADMIN_PASSWORD")
	}
	return &Account{Username: "admin", Password: adminPwd}, nil
}

// renderConfig renders scenarios/config.tmpl.scm with the given environment
// and writes it to debugDir/config.scm. Returns the absolute path.
func renderConfig(t *testing.T, env *Environment, debugDir string) string {
	t.Helper()

	tmpl, err := template.ParseFiles("scenarios/config.tmpl.scm")
	require.NoError(t, err)

	var buf bytes.Buffer
	require.NoError(t, tmpl.Execute(&buf, env))

	configPath := filepath.Join(debugDir, "config.scm")
	require.NoError(t, os.WriteFile(configPath, buf.Bytes(), 0o644))

	absPath, err := filepath.Abs(configPath)
	require.NoError(t, err)
	return absPath
}

// runScenarios renders the shared config template once, then executes each
// scenario-*.scm file found via scenarioGlob. prefixFiles are loaded (in order)
// between the config and the scenario (e.g. client helpers for feature tests).
func runScenarios(t *testing.T, env *Environment, scenarioGlob string, prefixFiles []string, debugDir string) {
	t.Helper()

	configPath := renderConfig(t, env, debugDir)

	tmEnvVars := []string{
		fmt.Sprintf("TEXMACS_HOME_PATH=%s", env.TMHomePath),
		fmt.Sprintf("TEXMACS_PATH=%s", env.TMPath),
	}

	// Resolve prefix files to absolute paths once.
	absPrefixFiles := make([]string, len(prefixFiles))
	for i, pf := range prefixFiles {
		abs, err := filepath.Abs(pf)
		require.NoError(t, err)
		absPrefixFiles[i] = abs
	}

	scenarios, err := filepath.Glob(scenarioGlob)
	require.NoError(t, err)
	require.NotEmpty(t, scenarios, "no scenarios matched %s", scenarioGlob)

	for _, scPath := range scenarios {
		scName := filepath.Base(scPath)
		if !strings.HasPrefix(scName, "scenario-") {
			continue
		}

		scPath := scPath
		testName := strings.TrimSuffix(scName, ".scm")
		t.Run(testName, func(tt *testing.T) {
			tt.Parallel()

			absScPath, err := filepath.Abs(scPath)
			require.NoError(tt, err)

			// Build script: load config, then prefix files, then scenario.
			// Each file is (load ...) so defines stay at top level.
			var script strings.Builder
			fmt.Fprintf(&script, "(load \"%s\")\n", configPath)
			for _, pf := range absPrefixFiles {
				fmt.Fprintf(&script, "(load \"%s\")\n", pf)
			}
			fmt.Fprintf(&script, "(load \"%s\")\n", absScPath)

			debugScript := filepath.Join(debugDir, testName+".scm")
			require.NoError(tt, os.WriteFile(debugScript, []byte(script.String()), 0o644))
			tt.Logf("script: %s", debugScript)

			cmd := exec.CommandContext(tt.Context(), env.TMExec, "--debug-io",
				"--headless",
				"--tls-no-verify",
				"-X", "-x", script.String())
			cmd.Env = tmEnvVars

			out, err := cmd.CombinedOutput()
			require.NoError(tt, err, "texmacs script failed:\n%s", string(out))

			tt.Log("output:\n" + string(out))
		})
	}
}

// TestLoad runs load scenarios from scenarios/load/.
// Scenarios simulate server activity without feature assertions.
func TestLoad(t *testing.T) {
	env, err := setupEnv()
	require.NoError(t, err)

	adminAccount, err := getAdminAccount()
	require.NoError(t, err)
	env.Accounts[adminAccount.Username] = adminAccount

	t.Logf("TEST_SEED=%d", env.Seed)

	debugDir := filepath.Join("debug", t.Name())
	require.NoError(t, os.MkdirAll(debugDir, 0o755))
	require.NoError(t, os.WriteFile(filepath.Join(debugDir, "SEED"),
		[]byte(strconv.FormatUint(env.Seed, 10)+"\n"), 0o644))

	runScenarios(t, env, "scenarios/load/scenario-*.scm", nil, debugDir)
}

// TestFeatures runs feature scenarios from scenarios/features/.
// Each scenario asserts correct server behaviour and fails the test on mismatch.
func TestFeatures(t *testing.T) {
	env, err := setupEnv()
	require.NoError(t, err)

	adminAccount, err := getAdminAccount()
	require.NoError(t, err)
	env.Accounts[adminAccount.Username] = adminAccount

	t.Logf("TEST_SEED=%d", env.Seed)

	debugDir := filepath.Join("debug", t.Name())
	require.NoError(t, os.MkdirAll(debugDir, 0o755))
	require.NoError(t, os.WriteFile(filepath.Join(debugDir, "SEED"),
		[]byte(strconv.FormatUint(env.Seed, 10)+"\n"), 0o644))

	runScenarios(t, env, "scenarios/features/scenario-*.scm",
		[]string{"fixture/helpers/client-helpers.scm"}, debugDir)
}
