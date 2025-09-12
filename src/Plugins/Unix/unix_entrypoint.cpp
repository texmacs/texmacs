
/******************************************************************************
* MODULE     : unix_entrypoint.cpp
* DESCRIPTION: Unix entry point for Unix
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_entrypoint.hpp"
#include "unix_system.hpp"
#include "boot.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"

#include <QCoreApplication>
#include <QDebug>
#include <QDir>

#include <vector>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <cstdio>
#include <iostream>
#include <thread>

#define EXPERIMENTAL_REEXEC_DETACHED 0

void setup_texmacs_path () {
  string environment_texmacs_path;
  if (texmacs_getenv ("TEXMACS_PATH", environment_texmacs_path)) return;
  url exedir = texmacs_get_application_directory ();
    if (test_texmacs_path (exedir * "TeXmacs")) {
    return;
  }
  if (test_texmacs_path (exedir * "usr/share/TeXmacs")) return;
  if (test_texmacs_path (exedir * "usr/local/share/TeXmacs")) return;
  if (test_texmacs_path (exedir * "../usr/share/TeXmacs")) return;
  if (test_texmacs_path ("/usr/share/TeXmacs")) return;
  if (test_texmacs_path ("/usr/local/share/TeXmacs")) return;
}

#if EXPERIMENTAL_REEXEC_DETACHED
class TeeThread {

public:
  TeeThread (int input_fd) : input_fd (input_fd) {
  }

  ~TeeThread () {
    for (size_t i = 0; i < to_close.size (); i++) {
      close (to_close[i]);
    }
  }

  TeeThread *add_output (int output_fd) {
    output_fds.push_back (output_fd);
    return this;
  }

  static TeeThread *from_stout () {
    std::fflush (stdout);
    std::cout.flush ();

    int p[2];
    if (pipe (p) == -1) {
      return nullptr;
    }

    // save stdout
    int saved_stdout = dup (STDOUT_FILENO);
    if (saved_stdout == -1) {
      close (p[0]);
      close (p[1]);
      return nullptr;
    }

    // redirect stdout to pipe
    if (dup2 (p[1], STDOUT_FILENO) == -1) {
      close (p[0]);
      close (p[1]);
      close (saved_stdout);
      return nullptr;
    }

    close (p[1]);

    setvbuf (stdout, nullptr, _IONBF, 0); // disable buffering for stdout
    return (new TeeThread (p[0]))->add_output (saved_stdout);
  }

  static TeeThread *from_sterr () {
    std::fflush (stderr);
    std::cerr.flush ();

    int p[2];
    if (pipe (p) == -1) {
      return nullptr;
    }

    // save stderr
    int saved_stderr = dup (STDERR_FILENO);
    if (saved_stderr == -1) {
      close (p[0]);
      close (p[1]);
      return nullptr;
    }

    // redirect stderr to pipe
    if (dup2 (p[1], STDERR_FILENO) == -1) {
      close (p[0]);
      close (p[1]);
      close (saved_stderr);
      return nullptr;
    }

    close (p[1]);

    setvbuf (stderr, nullptr, _IONBF, 0);
    return (new TeeThread (p[0]))->add_output (saved_stderr);
  }
  
  int loop () {
    char buffer[4096];
    while (true) {
      ssize_t count = read (input_fd, buffer, sizeof (buffer));
      if (count == -1) {
        if (errno == EINTR) continue;
        break;
      }
      if (count == 0) {
        break;
      }
      for (size_t i = 0; i < output_fds.size (); i++) {
        ssize_t written = 0;
        while (written < count) {
          ssize_t w = write (output_fds[i], buffer + written, count - written);
          if (w == -1) {
            if (errno == EINTR) continue;
            break;
          }
          written += w;
        }
      }
    }
    return 0;
  }

  void run_in_thread () {
    std::thread t ([this]() { this->loop (); delete this; });
    t.detach ();
  }

private:
  int input_fd;
  std::vector<int> output_fds, to_close;
};

int reexec_detached (int argc, char** argv) {
  if (getenv("TM_REEXEC")) {
    cout << "Hello from re-executed process" << LF;
    return;
  }

  int file_output = open ("/tmp/texmacs.log", O_WRONLY | O_CREAT | O_APPEND, 0644);
  if (file_output != -1) {
    cout << "Logging to /tmp/texmacs.log" << LF;
    TeeThread *tee_out = TeeThread::from_stout ();
    if (tee_out) {
      tee_out->add_output (file_output);
      tee_out->run_in_thread ();
    }
    TeeThread *tee_err = TeeThread::from_sterr ();
    if (tee_err) {
      tee_err->add_output (file_output);
      tee_err->run_in_thread ();
    }
  }

  cout << "Forking and detaching..." << LF;
  pid_t pid = fork ();
  if (pid < 0) {
    cout << "fork failed: " << strerror (errno) << LF;
    cout << "continuing normally" << LF;
    // fork failed. continue normally
    return;
  }

  if (pid > 0) {
    cout << "Hello from parent process, child pid is " << pid << LF;
    // keep TeeThreads alive and wait for child
    int status;
    waitpid (pid, &status, 0);
    // todo here : if texmacs exited with error, show a message box ?
    // todo here : telemetry ?
    if (WIFEXITED (status)) {
      cout << "child exited with status " << WEXITSTATUS (status) << LF;
      _exit (WEXITSTATUS (status));
    }
    if (WIFSIGNALED (status)) {
      cout << "child terminated by signal " << WTERMSIG (status) << LF;
      _exit (128 + WTERMSIG (status));
    }
    return 1;
  }

  setsid ();
  setenv ("TM_REEXEC", "1", 1);
  
  execv (argv[0], argv);
  _exit (127);

}
#endif

int main (int argc, char** argv) {
#if EXPERIMENTAL_REEXEC_DETACHED
  reexec_detached (argc, argv);
#endif

  texmacs_init_guile_hooks ();
  setup_texmacs_path ();
  if (get_env ("APPIMAGE") != "") {
    url usr_bin = url (get_env ("APPDIR")) * "usr/bin";
    url usr_local_bin = url (get_env ("APPDIR")) * "usr/local/bin";
    set_env ("PATH", get_env ("PATH") * ":" * as_string (usr_bin) * ":" * as_string (usr_local_bin));
  }
#if !defined (OS_MACOS) 
#if QT_VERSION < 0x060000
  if (get_env ("WAYLAND_DISPLAY") == "") {
    set_env ("QT_QPA_PLATFORM", "xcb"); // todo : remove ?
    set_env ("XDG_SESSION_TYPE", "x11");
  }
#endif
  // workaround for bug #66718 (Copy/paste not functioning in GNOME (Wayland))
  if (get_env ("WAYLAND_DISPLAY") != ""
      && occurs(get_env ("XDG_CURRENT_DESKTOP"), string("GNOME")))
    set_env ("QT_QPA_PLATFORM", "xcb");
#endif
  return texmacs_entrypoint (argc, argv);
}
