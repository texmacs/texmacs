<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Connecting to a <TeXmacs> server><label|sec-man-connect>

  Before using any collaborative features, you need to connect to a <TeXmacs>
  server. A server can be hosted by your institution, research group, or you
  can connect to a public server.

  To connect to a server:

  <\enumerate>
    <item>Go to <menu|Remote|Login> (or click the <icon|tm_cloud.xpm> cloud
    icon) to open the connection dialog.

    <item>Enter the server address (hostname or IP address) and optionally
    specify a port number if the server uses a non-standard port (the default
    is 6561).

    <item>If you already have an account, enter your username and password.
    Otherwise, you can create a new account (see the section below).

    <item>Click <menu|Connect> to establish the connection.
  </enumerate>

  Once connected, you will have access to all remote features. You can
  disconnect at any time using <menu|Remote|Logout>.

  The server supports both TLS-encrypted connections (<strong|highly
  recommended>) and legacy unencrypted connections (<strong|not
  recommended>). If security is important for your work, ensure that your
  server administrator has enabled TLS support.

  <\traverse>
    <branch|Creating and managing accounts|connect/man-accounts.en.tm>
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