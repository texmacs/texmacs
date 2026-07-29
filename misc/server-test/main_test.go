package servertest

import (
	"bytes"
	"crypto/rand"
	_ "embed"
	"encoding/binary"
	"errors"
	"flag"
	"fmt"
	"io"
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

var RootTmpl = template.New("root")

//go:embed scenarios/config.tmpl.scm
var ConfigTmplFile string
var ConfigTmpl = template.Must(RootTmpl.New("config").Parse(ConfigTmplFile))

//go:embed scenarios/config-instances.tmpl.scm
var ConfigInstancesTmplFile string
var ConfigInstancesTmpl = template.Must(RootTmpl.New("config-instances").Parse(ConfigInstancesTmplFile))

type Instance struct {
	Host     string
	Port     string
	Protocol string
}

type TmplConfig struct {
	Host     string
	Port     string
	Protocol string
	Client   int
	Accounts map[string]*Account
	Seed     uint64
	Timeout  int
}

type InstancesValue struct {
	Instances *[]Instance
}

func (v InstancesValue) String() string {
	instancesStr := ""
	if v.Instances != nil {
		for _, s := range *v.Instances {

			instancesStr = fmt.Sprintf("%s,%s", instancesStr, strings.Join([]string{s.Host, s.Port, s.Protocol}, ","))
		}
	}
	return ""
}

func (v InstancesValue) Set(s string) error {
	instancesStr := strings.Split(s, ",")
	*v.Instances = make([]Instance, 0, len(instancesStr))
	for _, instanceStr := range instancesStr {
		instanceValues := strings.Split(instanceStr, ":")

		if len(instanceValues) > 2 {
			*v.Instances = append(*v.Instances, Instance{Host: instanceValues[0], Port: instanceValues[1], Protocol: instanceValues[2]})
		} else if len(instanceValues) == 2 {
			*v.Instances = append(*v.Instances, Instance{Host: instanceValues[0], Port: instanceValues[1]})
		} else {
			return fmt.Errorf("too few values for instance '%s'", instanceStr)
		}
	}

	return nil
}

var instances = &[]Instance{}
var nclients *int = flag.Int("nclients", 1, "number of clients to spawn")
var timeout *int = flag.Int("tm-timeout", 5000, "script timeout to pass to TeXmacs")

func init() {
	flag.Var(&InstancesValue{instances}, "instances", "host:port:tls comma-separated list of running TeXmacs servers")
}

type Account struct {
	Username string
	Password string
}

type Environment struct {
	Instances []Instance
	NClients  int
	TMPath    string
	TMExec    string
	Accounts  map[string]*Account
	Seed      uint64
	Timeout   int
}

// TestConfig groups the parameters that vary between test categories
// (load, services).
type TestConfig struct {
	ScenarioGlob string
	ConfigTmpl   *template.Template
	PrefixFiles  []string
	DebugDir     string
}

func setupEnv() (*Environment, error) {
	tmPath := os.Getenv("TEXMACS_PATH")
	if tmPath == "" {
		return nil, errors.New("TEXMACS_PATH environment not set")
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

	if len(*instances) == 0 {
		*instances = append(*instances, Instance{
			Host:     "localhost",
			Port:     "6561",
			Protocol: "tls",
		})
	}

	return &Environment{
		Instances: *instances,
		NClients:  *nclients,
		TMPath:    tmPath,
		TMExec:    path.Join(tmPath, "bin/texmacs"),
		Accounts:  make(map[string]*Account),
		Seed:      seed,
		Timeout:   *timeout,
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
func renderConfig(t *testing.T, env *Environment, tc *TestConfig, instance Instance, clientIdx int) string {
	t.Helper()

	config := &TmplConfig{
		Host:     instance.Host,
		Port:     instance.Port,
		Protocol: instance.Protocol,
		Client:   clientIdx,
		Accounts: env.Accounts,
		Seed:     env.Seed,
		Timeout:  env.Timeout,
	}

	tc.ConfigTmpl.Funcs(template.FuncMap{"join": strings.Join})

	var buf bytes.Buffer
	require.NoError(t, tc.ConfigTmpl.Execute(&buf, config))

	configPath := filepath.Join(tc.DebugDir, fmt.Sprintf("%s-%s-config-client-%d.scm", instance.Host, instance.Port, clientIdx))
	require.NoError(t, os.WriteFile(configPath, buf.Bytes(), 0o644))

	absPath, err := filepath.Abs(configPath)
	require.NoError(t, err)
	return absPath
}

func runTexmacs(t *testing.T, env Environment, scriptName string) {
	cmd := exec.CommandContext(t.Context(), env.TMExec, "--debug-io",
		"--headless",
		"--tls-no-verify",
		"-X", "-x", fmt.Sprintf("(load \"%s\")\n", scriptName))

	tmHomePath, err := os.MkdirTemp("", "texmacs-client-*")
	require.NoError(t, err)

	cmd.Env = append(os.Environ(),
		fmt.Sprintf("TEXMACS_HOME_PATH=%s", tmHomePath),
		fmt.Sprintf("TEXMACS_PATH=%s", env.TMPath),
	)

	name := strings.TrimSuffix(path.Base(scriptName), path.Ext(scriptName))
	outfile, err := os.Create(path.Join(path.Dir(scriptName), fmt.Sprintf("%s-out.txt", name)))
	require.NoErrorf(t, err, "cannot create out file %s-out.txt", name)
	defer func() {
		errc := outfile.Close()
		require.NoError(t, errc)
	}()
	t.Logf("log file at %s", outfile.Name())

	w := io.MultiWriter(os.Stdout, outfile)
	cmd.Stdout = w
	cmd.Stderr = w

	t.Logf("running %s", cmd.String())

	err = cmd.Run()
	require.NoError(t, err)
}

// runScenarios renders a config template once, then executes each
// scenario-*.scm file found via tc.ScenarioGlob. tc.PrefixFiles are loaded
// (in order) between the config and the scenario.
func runScenarios(t *testing.T, env *Environment, tc *TestConfig) {
	t.Helper()

	require.NoError(t, os.MkdirAll(tc.DebugDir, 0o755))
	require.NoError(t, os.WriteFile(filepath.Join(tc.DebugDir, "SEED"),
		[]byte(strconv.FormatUint(env.Seed, 10)+"\n"), 0o644))

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
		for _, instance := range env.Instances {
			for i := range env.NClients {
				configPath := renderConfig(t, env, tc, instance, i)
				testName := fmt.Sprintf("%s-client-%d-%s_%s", strings.TrimSuffix(scName, ".scm"), i, instance.Host, instance.Port)
				t.Run(testName, func(tt *testing.T) {
					tt.Parallel()

					absScPath, err := filepath.Abs(scPath)
					require.NoError(tt, err)

					var script strings.Builder
					fmt.Fprintf(&script, "(load \"%s\")\n", configPath)
					for _, pf := range absPrefixFiles {
						fmt.Fprintf(&script, "(load \"%s\")\n", pf)
					}
					fmt.Fprintf(&script, "(load \"%s\")\n", absScPath)

					runnerScript := filepath.Join(tc.DebugDir, testName+".scm")
					require.NoError(tt, os.WriteFile(runnerScript, []byte(script.String()), 0o644))
					tt.Logf("script: %s", runnerScript)

					runTexmacs(tt, *env, runnerScript)
				})
			}
		}
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
		ConfigTmpl:   ConfigTmpl,
		DebugDir:     filepath.Join("debug", t.Name()),
	})
}

// TestServices runs service scenarios from scenarios/services/.
// Each scenario tests tm-service definitions through client-server RPC
// and fails the test on assertion mismatch.
func TestServices(t *testing.T) {
	require.NotNil(t, instances)
	env, err := setupEnv()
	require.NoError(t, err)

	adminAccount, err := getAdminAccount()
	require.NoError(t, err)
	env.Accounts[adminAccount.Username] = adminAccount

	t.Logf("TEST_SEED=%d", env.Seed)

	runScenarios(t, env, &TestConfig{
		ScenarioGlob: "scenarios/services/scenario-*.scm",
		ConfigTmpl:   ConfigTmpl,
		PrefixFiles:  []string{"fixture/helpers/test-helpers.scm", "fixture/helpers/client-helpers.scm"},
		DebugDir:     filepath.Join("debug", t.Name()),
	})
}
