<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Security tips>

  When running a <TeXmacs> server, consider the following security practices:

  <\itemize>
    <item><strong|Always use TLS> for any server accessible over a network.
    Without TLS, passwords and content are transmitted in plain text.

    <item><strong|Change the default admin password immediately> after
    starting the server for the first time.

    <item><strong|Use strong password requirements> to protect user accounts.

    <item><strong|Configure failed login limits> to prevent brute-force
    attacks.

    <item><strong|Disable unnecessary services>. If you don't need remote
    account creation, disable it.

    <item><strong|Use CA-signed certificates> for public servers to avoid
    certificate warnings for users.

    <item><strong|Regularly review user accounts> and remove or suspend
    accounts that are no longer needed.

    <item><strong|Keep <TeXmacs> updated> to receive security fixes.

    <item><strong|Backup server data regularly>, including the database and
    user files in <verbatim|$TEXMACS_HOME_PATH/server/>.
  </itemize>

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