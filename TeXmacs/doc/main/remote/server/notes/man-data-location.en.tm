<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Server data location>

  All server data is stored in <verbatim|$TEXMACS_HOME_PATH/server/>,
  typically <verbatim|~/.TeXmacs/server/>:

  <\description>
    <item*|cert.pem, key.pem>TLS certificate and private key.

    <item*|users.scm>User account information.

    <item*|pending-users.scm>Accounts awaiting email confirmation.

    <item*|reset-credentials-users.scm>Pending password reset requests.

    <item*|license.tm>Server license agreement.

    <item*|email-new-account.txt>Email template for new accounts.

    <item*|email-reset-credentials.txt>Email template for password resets.
  </description>

  The server database (containing files, chat messages, and other resources)
  is stored in the global <TeXmacs> database.

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