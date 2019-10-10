# Python plugin
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Installation
Python is a built-in plugin in GNU TeXmacs.
Sometimes, the built-in one does not work or does not meet your requirement.
You may install the latest or the specific version of the plugin manually
to `$TEXMACS_HOME_PATH/plugins`. The plugin under `$TEXMACS_HOME_PATH` will
override the system one.

For GNU/Linux and macOS:
``` bash
git clone git@github.com:texmacs/python.git $HOME/.TeXmacs/plugins/python
```

For Windows:
``` bash
git clone git@github.com:texmacs/python.git %APPDATA%\TeXmacs\plugins\python
```

To switch to a specific version, use `git checkout` or modify it manually.

## Change the Python Interpreter
Install the Python plugin to `$TEXMACS_HOME_PATH/plugins` and then change
the python launcher in `$TEXMACS_HOME_PATH/plugins/python/progs/init-python.scm`.

If you have permissions for `$TEXMACS_PATH/plugins/python/progs/init-python.scm`, you may also
change the system one.

Here is the detailed example of changing from python to python3:

``` scheme
(define (python-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append "python \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")
      (string-append "python \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")))
```

To use python3, just replace the above code with:

``` scheme
(define (python-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append "python3 \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")
      (string-append "python3 \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")))
```