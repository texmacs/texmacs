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

// TestConfig groups the parameters that vary between test categories
// (load, services).
type TestConfig struct {
	ScenarioGlob string
	ConfigTmpl   string
	PrefixFiles  []string
	DebugDir     string
}

func setupEnv() (*Environment, error) {
	tmPath := os.Getenv("TEXMACS_PATH")
	if tmPath == "" {
		return nil, errors.New("TEXMACS_PATH environment not set")
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

	host := os.Getenv("TMSERVER_HOST")
	if host == "" {
		host = "localhost"
	}
	port := os.Getenv("TMSERVER_PORT")
	if port == "" {
		port = "6561"
	}
	protocol := os.Getenv("TMSERVER_PROTOCOL")
	if protocol == "" {
		protocol = "tls"
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
		Host:       host,
		Port:       port,
		Protocol:   protocol,
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

// renderConfig renders tc.ConfigTmpl with the given environment
// and writes it to tc.DebugDir/config.scm. Returns the absolute path.
func renderConfig(t *testing.T, env *Environment, tc *TestConfig) string {
	t.Helper()

	tmpl, err := template.ParseFiles(tc.ConfigTmpl)
	require.NoError(t, err)

	var buf bytes.Buffer
	require.NoError(t, tmpl.Execute(&buf, env))

	configPath := filepath.Join(tc.DebugDir, "config.scm")
	require.NoError(t, os.WriteFile(configPath, buf.Bytes(), 0o644))

	absPath, err := filepath.Abs(configPath)
	require.NoError(t, err)
	return absPath
}

// runScenarios renders a config template once, then executes each
// scenario-*.scm file found via tc.ScenarioGlob. tc.PrefixFiles are loaded
// (in order) between the config and the scenario.
func runScenarios(t *testing.T, env *Environment, tc *TestConfig) {
	t.Helper()

	require.NoError(t, os.MkdirAll(tc.DebugDir, 0o755))
	require.NoError(t, os.WriteFile(filepath.Join(tc.DebugDir, "SEED"),
		[]byte(strconv.FormatUint(env.Seed, 10)+"\n"), 0o644))

	configPath := renderConfig(t, env, tc)

	tmEnvVars := []string{
		fmt.Sprintf("TEXMACS_HOME_PATH=%s", env.TMHomePath),
		fmt.Sprintf("TEXMACS_PATH=%s", env.TMPath),
	}

	// Resolve prefix files to absolute paths once.
	absPrefixFiles := make([]string, len(tc.PrefixFiles))
	for i, pf := range tc.PrefixFiles {
		abs, err := filepath.Abs(pf)
		require.NoError(t, err)
		absPrefixFiles[i] = abs
	}

	scenarios, err := filepath.Glob(tc.ScenarioGlob)
	require.NoError(t, err)
	require.NotEmpty(t, scenarios, "no scenarios matched %s", tc.ScenarioGlob)

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

			debugScript := filepath.Join(tc.DebugDir, testName+".scm")
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

	runScenarios(t, env, &TestConfig{
		ScenarioGlob: "scenarios/load/scenario-*.scm",
		ConfigTmpl:   "scenarios/config.tmpl.scm",
		DebugDir:     filepath.Join("debug", t.Name()),
	})
}

// TestServices runs service scenarios from scenarios/services/.
// Each scenario tests tm-service definitions through client-server RPC
// and fails the test on assertion mismatch.
func TestServices(t *testing.T) {
	env, err := setupEnv()
	require.NoError(t, err)

	adminAccount, err := getAdminAccount()
	require.NoError(t, err)
	env.Accounts[adminAccount.Username] = adminAccount

	t.Logf("TEST_SEED=%d", env.Seed)

	runScenarios(t, env, &TestConfig{
		ScenarioGlob: "scenarios/services/scenario-*.scm",
		ConfigTmpl:   "scenarios/config.tmpl.scm",
		PrefixFiles:  []string{"fixture/helpers/test-helpers.scm", "fixture/helpers/client-helpers.scm"},
		DebugDir:     filepath.Join("debug", t.Name()),
	})
}
