<TeXmacs|1.99.19>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Installation>

  The <samp|matlab-plugin> is a built-in plugin in GNU <TeXmacs>. It assumes
  that <name|Matlab> is installed on your system. If
  <menu|Insert|Session|Matlab> is not available, do the
  following<space|0.2spc>:

  <\enumerate>
    <item>Check if <name|Matlab> is installed, if not, you will need to
    install <name|Matlab> first.

    <item>Check if command line <shell|matlab> is in your system path
    (<verbatim|PATH>)

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-exists-in-path? "matlab")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>

    if not (<abbr|i.<space|0.2spc>e.> <verbatim|#f>), add the directory where
    <shell|matlab> resides to your <verbatim|PATH>.

    <item>The way to do this on <name|OS<space|0.6spc>X> is to create a
    softlink to it by issuing\ 

    <code|<\code*>
      sudo ln -sf /Applications/MATLAB_R2020a.app/bin/matlab
      /usr/local/bin/matlab
    </code*>>

    This should work in a similar way on other Unix systems and with other
    versions of <name|Matlab>. Now that you have <code*|matlab> in your
    <code*|PATH>, <menu|Insert|Session|Matlab> will show up.

    <item>As you probably need to customize the <samp|matlab-plugin>, you
    will want to install it under the <code*|$TEXMACS_HOME_PATH>:

    <code|<\code*>
      # For GNU/Linux or macOS

      git clone git@github.com:texmacs/matlab.git
      $HOME/.TeXmacs/plugins/matlab

      \;

      # For Windows

      git clone git@github.com:texmacs/matlab.git
      %APPDATA%\\TeXmacs\\plugins/matlab
    </code*>>

    Users who do not know about Git can also download the zip-file from
    Github and unzip <shell|master.zip> to the corresponding directory.

    <item>Then, it is critical to make sure that the <name|Matlab>-startup
    script <verbatim|tm_matlab> in <code*|$TEXMACS_HOME_PATH/plugins/matlab/bin>
    looks like this<space|0.2spc>:\ 
  </enumerate>

  <\shell-code>
    <code|<\code*>
      # For GNU/Linux or macOS

      \;

      \ #!/bin/bash

      \ \ \ echo -ne "\\002verbatim:"

      \ \ \ PLUGIN_CODE_PATH=$TEXMACS_PATH/plugins/matlab/code/

      \ \ \ export MATLABPATH="$MATLABPATH:$PLUGIN_CODE_PATH"

      \ \ \ matlab -nodesktop -nosplash -r tmrepl

      \;
    </code*>>
  </shell-code>

  \;

  <\shell-code>
    <code|<\code*>
      # For Windows

      \ \ 

      ?? tm_matlab.bat ?? Isn't there a bash shell for Windows?

      \;
    </code*>>
  </shell-code>

  If not, you may <strong|have> to <strong|edit> <verbatim|tm_matlab>
  accordingly. This ensures that <verbatim|PLUGIN_CODE_PATH> is added to the
  original <verbatim|MATLABPATH> as defined by <name|Matlab>, and that both
  are exported when launching a <verbatim|Matlab>-session from within
  <TeXmacs>. Thus, any user defined <name|Matlab> configuration in a
  <verbatim|startup.m>, if there is one, will be considered. <em|Now> you're
  up and running.

  <tmdoc-copyright|2020\U2021|Darcy Shen and Tilda <abbr|A.> Steiner>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>