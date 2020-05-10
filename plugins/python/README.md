# Python plugin
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Installation
It is a built-in plugin in TeXmacs. If you need to customize it, please install it
under the `$TEXMACS_HOME_PATH`:

``` bash
# For GNU/Linux or macOS:
git clone git@github.com:texmacs/python.git $HOME/.TeXmacs/plugins/python

# For Windows:
git clone git@github.com:texmacs/python.git %APPDATA%\TeXmacs\plugins\python
```
## Change the Python Interpreter
Install the Python plugin to `$TEXMACS_HOME_PATH/plugins` and then change
the python launcher in `$TEXMACS_HOME_PATH/plugins/python/progs/init-python.scm`.

If you have permissions for `$TEXMACS_PATH/plugins/python/progs/init-python.scm`, you may also change the system one.

Here is the detailed example of changing the python command:

``` scheme
(define (python-command) "python3")
```

By default, we use `python3`. To use a specific python version, just change the return value of the `(python-commond)` function.

``` scheme
(define (python-command) "/path/to/python")
```