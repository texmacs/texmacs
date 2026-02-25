<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Remote tools and collaborative
  editing><label|sec-man-collaborative>

  <TeXmacs> includes a powerful client-server architecture that enables
  real-time collaboration, remote file management, and instant messaging.
  This system allows multiple users to work together on documents, share
  resources, and communicate through integrated chat rooms. The remote tools
  integrate seamlessly with <TeXmacs>, making collaboration feel natural and
  intuitive.

  In what follows, a <em|remote resource> refers to a document that is stored
  and managed on a remote server and which can potentially be accessed and
  edited simultaneously by several users. Currently, there are three types of
  remote resources:

  <\itemize>
    <item><strong|Files and directories> work in a similar way as your local
    files and directories, except that they are stored on a remote server and
    that one may control very precisely which other users have read and/or
    write access to the remote documents. Moreover, version control is
    automatically enabled for all remote files, which allows you to examine
    the full history of such documents.

    <item><strong|Live documents> are files that can be accessed
    simultaneously and in real time by several users. This is often used in
    combination with visio conference software in order to share written
    notes, formulas, and/or figures on-the-fly. There is an unlimited number
    of users who can access live documents simultaneously, so this feature
    can also be used for broadcasting a live conference or interactive
    teaching.

    <item><strong|Chat rooms> are a light-weight facility for instant
    messaging. You may set up your own chat rooms for selected groups of
    friends. Any user also has a personal mailbox (which technically works in
    the same way as a chat room) for private messages and invitations to open
    shared resources.
  </itemize>

  In order to use the remote tools, you must enable the <menu|Tools|Remote
  tool>. This adds the <menu|Remote> menu and toolbar icons, which provide
  all the actions to communicate with a remote server and access its
  resources. You should next <hlink|connect to a remote <TeXmacs>
  server|man-connect.en.tm> and create an account. After logging in, the user
  interface is organized as follows:

  <\description-dash>
    <item*|<icon|tm_cloud.svg>> Connection facility: login, logout, create
    account, reset credentials.

    <item*|<icon|tm_cloud_home.svg>>Remote resources: home directory, chat
    rooms, shared resources, live documents, etc.

    <item*|<icon|tm_cloud_mail.svg>>Mail: read and write messages.

    <item*|<icon|tm_cloud_file.svg>>Manage the current remote resource:
    change permissions, sharing with friends, etc.

    <item*|<icon|tm_cloud_dir.svg>>Manage a remote directory: create new
    resources, share with other users, synchronize.
  </description-dash>

  Below, we describe the remote tools from the perspective of a user. System
  administrators or power users may also wish to <hlink|set up and manage
  their own <TeXmacs> server|man-server.en.tm>.

  <\traverse>
    <branch|Connecting to a <TeXmacs> server|man-connect.en.tm>

    <branch|Messages and chat rooms|man-chat.en.tm>

    <branch|Shared resources|man-share.en.tm>

    <branch|Permissions and access control|man-permissions.en.tm>

    <branch|Notes and troubleshooting|man-remote-notes.en.tm>
  </traverse>

  <tmdoc-copyright|2025, 2026|Joris van der Hoeven|Robin Wils>

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