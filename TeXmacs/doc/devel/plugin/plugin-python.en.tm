<TeXmacs|1.99.11>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Example of a plug-in with Python code>

  <paragraph*|The <verbatim|pyminimal> plug-in>

  Consider the example of the <verbatim|pyminimal> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  It consists of the following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|pyminimal/progs/init-pyminimal.scm>

    \ \ \ \ <example-plugin-link|pyminimal/src/minimal.py>
  </verbatim>

  In order to try the plug-in, you first have to recursively copy the
  directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/pyminimal
  </verbatim>

  to <verbatim|$TEXMACS_PATH/progs> or <verbatim|$TEXMACS_HOME_PATH/progs>.

  When relaunching <TeXmacs>, the plug-in should now be automatically
  recognized.

  <paragraph*|How it works: The Scheme Part>

  The <verbatim|pyminimal> plug-in demonstrates a minimal interface between
  <TeXmacs> and an extern program in python. The initialization file
  <verbatim|init-pyminimal.scm> essentially contains the following code:

  <\scm-code>
    (define (python-launcher)

    \ \ (if (url-exists? "$TEXMACS_HOME_PATH/plugins/pyminimal")

    \ \ \ \ \ \ (string-append "python \\""

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (getenv "TEXMACS_HOME_PATH")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "/plugins/pyminimal/src/minimal.py\\"")

    \ \ \ \ \ \ (string-append "python \\""

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (getenv "TEXMACS_PATH")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "/plugins/pyminimal/src/minimal.py\\"")))

    \;

    (plugin-configure pyminimal

    \ \ (:require (url-exists-in-path? "python"))

    \ \ (:launch ,(python-launcher))

    \ \ (:session "PyMinimal"))
  </scm-code>

  The <scm|:require> option checks whether <shell|python> indeed exists in
  the path (so this will fail if you did not have python installed). The
  <scm|:launch> option specifies how to launch the extern program. The
  <verbatim|:session> option indicates that it will be possible to create
  sessions for the <verbatim|pyminimal> plug-in using
  <menu|Insert|Session|PyMinimal>.

  The <scm|python-launcher> function will be evaluated and return the proper
  command to launcher the extern program. If
  <shell|$TEXMACS_HOME_PATH/plugins/pyminimal> exists, it would be

  <\shell-code>
    python "$TEXMACS_HOME_PATH/plugins/pyminimal/src/minimal.py"
  </shell-code>

  otherwise,

  <\shell-code>
    python "$TEXMACS_PATH/plugins/pyminimal/src/minimal.py"
  </shell-code>

  The environment variables will be replaced in runtime. Sometimes,
  <shell|$TEXMACS_HOME_PATH> and <shell|$TEXMACS_PATH> may contain spaces, as
  a result, we quote the path using the double quotes.

  <paragraph|How it works: The Python Part>

  Using the Python interpreter, we do not need to compile the code. And most
  of the time, python code can be interpreted without any modification under
  multiple platforms. The built-in python libraries are really helpful and
  handy.

  Many <TeXmacs> built-in plugins are written in Python, and we have
  carefully organized the code and reused the common part named <name|tmpy>.

  <\python-code>
    import os

    import sys

    from os.path import exists

    tmpy_home_path = os.environ.get("TEXMACS_HOME_PATH") + "/plugins/tmpy"

    if (exists (tmpy_home_path)):

    \ \ \ \ sys.path.append(os.environ.get("TEXMACS_HOME_PATH") +
    "/plugins/")

    else:

    \ \ \ \ sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")
  </python-code>

  The first part of the code just add <shell|$TEXMACS_HOME_PATH/plugins> or
  <shell|$TEXMACS_PATH/plugins> to the python path for importing the
  <name|tmpy> package.

  <\python-code>
    from tmpy.protocol \ \ \ \ \ \ \ import *

    from tmpy.compat \ \ \ \ \ \ \ \ \ import *

    \;

    flush_verbatim ("Hi there!")

    \;

    while True:

    \ \ \ \ line = tm_input()

    \ \ \ \ if not line:

    \ \ \ \ \ \ \ \ pass

    \ \ \ \ else:

    \ \ \ \ \ \ \ \ flush_verbatim ("You typed " + line)
  </python-code>

  <python|flush_verbatim> is provided by <python|tmpy.protocol> which is a
  subpackage for interaction with <TeXmacs> server in Python.

  <python|tm_input> is provided by <python|tmpy.compat> which is a subpackage
  for compatibility within Python 2 and 3. For built-in plugins written in
  Python, it would be better to support more Python version. For example, in
  some desktop environments, the <shell|python> command may redirect to
  <shell|python2.6>.

  <paragraph|Comparison with <c++>>

  This demo plugin is a Python implementation of the well-documented
  <hlink|minimal plugin|plugin-binary.en.tm> written in <c++>. Here is the
  summary of the differences:

  <\itemize>
    <item><verbatim|pyminimal> requires the <name|Python> interpreter,
    <verbatim|minimal> needs to be compiled and linked

    <item><verbatim|pyminimal> is easier to install than <verbatim|minimal>

    <item><verbatim|pyminimal> reuses the common part related to <TeXmacs>

    <item><verbatim|pyminimal> has better compatibility for <name|Linux>,
    <name|Windows> and <name|macOS>
  </itemize>

  <tmdoc-copyright|2019|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>