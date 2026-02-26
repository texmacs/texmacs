<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Logging>

  The server logs events at different severity levels:

  <\description>
    <item*|Emergency, Alert, Critical, Error>Serious issues requiring
    attention.

    <item*|Warning>Important notices such as password resets.

    <item*|Notice, Info, Debug>Informational messages for troubleshooting.
  </description>

  Server logs are reported to the correct logging facility depending on the
  OS the server is running.

  More verbose debugging information can be activated with the following
  command line flags:

  <verbatim|--debug-gnutls --debug-io --debug-remote --debug-sockets>

  Which are also available in the Debugging tool menu in the <TeXmacs>
  interface.

  <tmdoc-copyright|2025|Robin Wils>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|false>
  </collection>
</initial>