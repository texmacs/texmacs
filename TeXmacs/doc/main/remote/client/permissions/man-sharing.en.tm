<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Inviting and sharing resources>

  <TeXmacs> provides convenient ways to share resources with specific users.

  <strong|Important.> Before inviting users or sharing resources, you should
  first set the appropriate permissions. Users cannot access a resource
  unless they have been granted read (and write, if needed) permissions on
  that resource.

  To share a resource with collaborators, you must first set permissions,
  then share:

  <\enumerate>
    <item>Open the resource in <TeXmacs>.

    <item>Select <menu|Remote|Permissions> and grant readable (and writable
    if they need to edit) access to the users you want to share with.

    <item>Select <menu|Remote|Share>. This menu item appears when viewing the
    resource.

    <item>Choose the users to share with from the list. Only users with
    permissions will appear in this list.

    <item>Click <menu|Send> to send the sharing notification.
  </enumerate>

  The share feature sends individual private messages to each selected user
  with a link to the resource. These messages appear in each user's private
  message inbox.

  <paragraph|How sharing notifications work>

  When you use <menu|Remote|Share> or <menu|Remote|Invite>, the system sends
  individual private messages to each selected user. These messages contain a
  clickable link to the shared resource and appear in each user's private
  message inbox (accessed via <menu|Remote|Incoming messages>).

  Private messages are implemented as personal chat rooms (internally named
  <samp|mail-{username}>), ensuring that each user receives their
  notification privately and can access shared resources at their
  convenience.

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