<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|User management>

  Administrator users can manage user accounts through the <TeXmacs>
  interface:

  <\enumerate>
    <item>Connect to the server and log in with an admin account.

    <item>Select <menu|Remote|User Management>.

    <item>The accounts list shows all registered users.

    <item>Select a user and click <menu|Edit> to modify their account.

    <item>Click <menu|Add> to create a new account.
  </enumerate>

  <paragraph|Creating accounts>

  When creating a new user account, fill in the required information:

  <\description>
    <item*|Pseudo>The username (must be unique)

    <item*|Full name>The user's display name

    <item*|Email>The user's email address

    <item*|Administrator>Toggle to grant admin privileges

    <item*|Password>The initial password
  </description>

  <paragraph|Editing accounts>

  When editing an existing account, you can modify:

  <\itemize>
    <item>Full name and email

    <item>Administrator status

    <item>Suspended status (to temporarily disable the account)

    <item>Password (change, add, or delete)
  </itemize>

  <paragraph|Suspending accounts>

  Suspended accounts cannot log in to the server. To suspend an account,
  enable the <menu|Suspended> toggle in the account editor. To resume a
  suspended account, disable the toggle.

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