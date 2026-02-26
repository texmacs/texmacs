<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Using a CA-signed certificate>

  For public servers, you should obtain a certificate signed by a trusted
  Certificate Authority (CA). To use a CA-signed certificate:

  <\enumerate>
    <item>Obtain the certificate and private key from your CA.

    <item>Save them as <verbatim|cert.pem> and <verbatim|key.pem> in
    <verbatim|$TEXMACS_HOME_PATH/server/>.

    <item>Start the server normally.
  </enumerate>

  CA-signed certificates are automatically trusted by clients without
  additional configuration.

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