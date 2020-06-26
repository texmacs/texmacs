<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Installation>

  It is a built-in plugin in GNU <TeXmacs>. If <menu|Insert|Session|Octave>
  is not available, do the following steps to fix it:

  <\enumerate>
    <item>Check if GNU Octave is installed, if not, please install GNU Octave
    (5.x) first

    <item>Check if the command line <shell|octave-cli> is available

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-exists-in-path? "octave-cli")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>

    if not, please add the directory where <shell|octave-cli> exists to the
    system path.
  </enumerate>

  If you need to customize it, please install it under the
  <code*|$TEXMACS_HOME_PATH>:

  <\shell-code>
    <code|<\code*>
      # For GNU/Linux or macOS

      git clone git@github.com:texmacs/octave.git
      $HOME/.TeXmacs/plugins/octave

      \;

      # For Windows

      git clone git@github.com:texmacs/octave.git
      %APPDATA%\\TeXmacs\\plugins/octave
    </code*>>
  </shell-code>

  For users who does not know about Git, please download the zip from Github
  and unzip the <shell|master.zip> to the corresponding directory.

  <tmdoc-copyright|2020|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>