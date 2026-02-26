<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Starting the server>

  To manage the server, you must first enable the remote tools via
  <menu|Tools|Remote tool>. This adds the <menu|Remote> menu, which contains
  server controls when no server is running.

  To start the server:

  <\enumerate>
    <item>Go to <menu|Remote|Start server>.

    <item>If TLS support is available and no certificate exists, then you
    will be prompted to create one (see <hlink|TLS
    certificates|../man-certificates.en.tm>).

    <item>When the server is started for the first time, a default
    administrator account is created automatically (see <hlink|Default admin
    account|man-default-admin.en.tm>).
  </enumerate>

  When the server is running, the <menu|Remote> menu displays its status and
  the port number in use (e.g., <samp|Server running on port 6561>).

  <paragraph|Server icon>

  When a local server is running, a server icon <icon|tm_cloud_server.xpm>
  appears in the toolbar. Clicking this icon opens a menu with server
  controls.

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