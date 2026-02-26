<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Server preferences><label|sec-server-preferences>

  Server configuration settings are stored in <TeXmacs> preferences and can
  be modified through the remote preferences form (for administrator users
  connected to the server) or by editing preferences directly. The settings
  are organized into several categories.

  <paragraph|Connection settings>

  <\description>
    <item*|Port number>The TCP port the server listens on. Default is
    <verbatim|6561>. Change this if the default port conflicts with other
    services.

    <item*|Contact timeout>Time in milliseconds to wait for initial client
    contact. Default is <verbatim|10000> (10 seconds).

    <item*|Connection timeout>Time in seconds before an idle connection is
    closed. Default is <verbatim|120> (2 minutes).
  </description>

  <paragraph|Authentication settings>

  <\description>
    <item*|Anonymous via TLS>When enabled, allows clients to connect
    anonymously over TLS for account creation and login. This should be
    enabled for public servers.
  </description>

  <paragraph|Account security>

  <\description>
    <item*|Failed login limit>Number of failed login attempts before an
    account is temporarily suspended. Default is <verbatim|3>. Set to
    <verbatim|0> to disable this protection.

    <item*|Failed login delay>Time in seconds before a suspended account can
    attempt login again. Default is <verbatim|3600> (1 hour).

    <item*|Account confirmation delay>Time in seconds for email confirmation
    of new accounts. Default is <verbatim|-1> (disabled, accounts are created
    immediately). Set to a positive value to require email confirmation.

    <item*|Credentials resetting delay>Time in seconds during which a
    password reset code is valid. Default is <verbatim|3600> (1 hour).
  </description>

  <paragraph|Services>

  These toggles control which services are available to remote clients:

  <\description>
    <item*|Remote password login>Allows users to log in with username and
    password.

    <item*|Remote account creation>Allows new users to create accounts
    remotely. Disable this for invite-only servers.

    <item*|Remote credentials resetting>Allows users to reset their password
    via email. Requires a configured mail command.
  </description>

  <paragraph|Password settings>

  <\description>
    <item*|Password encoding>The algorithm used to hash passwords. Options
    include <verbatim|clear> (not recommended), <verbatim|sha256>,
    <verbatim|sha512>, and <verbatim|pbkdf2> (most secure, requires GnuTLS).

    <item*|Require strong passwords>When enabled, passwords must contain at
    least 10 characters including uppercase, lowercase, digit, and symbol.

    <item*|Automatic encoding update>When enabled, passwords are re-encrypted
    with the current encoding algorithm upon successful login.
  </description>

  <paragraph|Resetting preferences>

  To reset all server preferences to their defaults, use <menu|Remote|Reset
  preferences> while the server is running locally.

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