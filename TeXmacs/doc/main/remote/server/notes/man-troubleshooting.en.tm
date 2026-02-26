<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Troubleshooting>

  <\description>
    <item*|Server fails to start>Check that the configured port is not
    already in use. Wait for a previous process releases the port, or use a
    different port number.

    <item*|TLS errors>Verify that the certificate and key files exist and are
    valid. Try regenerating the certificate.

    <item*|Users cannot connect or create an account>Ensure the firewall
    allows connections on the server port. Verify that the server address is
    reachable from the client. Verify the remote login and/or remote account
    creation are activated.

    <item*|Email not working>Check the mail command configuration. Verify
    that the mail system is properly configured on the server.

    <item*|Lost admin password>Use <menu|Remote|Reset admin password> while
    running the server locally to generate a new password.

    <item*|Account suspended>Check the failed login limit settings. Use the
    account editor to unsuspend the account if needed.
  </description>

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