# SageMath
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Installation
SageMath is a built-in plugin in GNU TeXmacs.
Sometimes, the built-in one does not work or does not meet your requirement.
You may install the latest or the specific version of the plugin manually
to `$TEXMACS_HOME_PATH/plugins`. The plugin under `$TEXMACS_HOME_PATH` will
override the system one.

For GNU/Linux and macOS:
``` bash
git clone https://github.com/texmacs/sage.git $HOME/.TeXmacs/plugins/sage
```

For Windows:
``` bash
git clone https://github.com/texmacs/sage.git %APPDATA%\TeXmacs\plugins\sage
```

To switch to a specific version, use `git checkout` or modify it manually.

If Github is not available, just replace github with gitee. For example:
```
git clone https://gitee.com/texmacs/sage.git %APPDATA%\TeXmacs\plugins\sage
```
