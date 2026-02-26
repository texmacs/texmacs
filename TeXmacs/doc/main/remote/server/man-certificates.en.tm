<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|TLS certificates><label|sec-certificates>

  TLS (Transport Layer Security) encrypts communication between clients and
  the server, protecting passwords and document content from eavesdropping.
  Using TLS is strongly recommended for any server accessible over a network.

  The server requires two files for it to be configured as TLS:

  <\itemize>
    <item><verbatim|cert.pem>: The server certificate

    <item><verbatim|key.pem>: The private key
  </itemize>

  These files are stored in <verbatim|$TEXMACS_HOME_PATH/server/> (typically
  <verbatim|~/.TeXmacs/server/> for Linux and macOS).

  <\traverse>
    <branch|Creating a self-signed certificate|certificates/man-self-signed.en.tm>

    <branch|Using a CA-signed certificate|certificates/man-ca-signed.en.tm>
  </traverse>

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