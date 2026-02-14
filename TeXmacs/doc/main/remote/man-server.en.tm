<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Setting up a TeXmacs server><label|sec-man-server>

  <TeXmacs> includes a built-in server that enables collaborative features
  such as remote file storage, live documents, and chat rooms. This document
  explains how to set up, configure, and administer a <TeXmacs> server,
  either locally on your machine or for a team.

  <section|Overview>

  The <TeXmacs> server provides:

  <\itemize>
    <item>remote file storage with version history,

    <item>live collaborative document editing,

    <item>chat rooms and messaging,

    <item>user account management,

    <item>TLS encryption for secure connections.
  </itemize>

  You can run a server on your local machine for personal use or testing, or
  set up a dedicated server for a team or organization. Server administration
  can be performed locally through the <TeXmacs> interface or remotely by
  administrator users.

  <section|Starting and stopping the server>

  To manage the server, you must first enable the remote tools via
  <menu|Tools|Remote tool>. This adds the <menu|Remote> menu, which contains
  server controls when no server is running.

  <subsection|Starting the server>

  To start the server:

  <\enumerate>
    <item>Go to <menu|Remote|Start server>.

    <item>If TLS support is available and no certificate exists, then you
    will be prompted to create one (see <hlink|TLS
    certificates|#sec-certificates>).

    <item>When the server is started for the first time, a default
    administrator account is created automatically (see <hlink|Default admin
    account|#sec-default-admin>).
  </enumerate>

  When the server is running, the <menu|Remote> menu displays its status and
  the port number in use (e.g., <samp|Server running on port 6561>).

  <subsection|Stopping and restarting>

  When the server is running, the following options are available at the
  <menu|Remote> menu:

  <\description>
    <item*|Stop server>Stops the server immediately. All connected clients
    will be disconnected.

    <item*|Restart server>Stops and then restarts the server. This is useful
    after changing configuration settings that require a restart.
  </description>

  <subsection|Server icon>

  When a local server is running, a server icon <icon|tm_cloud_server.xpm>
  appears in the toolbar. Clicking this icon opens a menu with server
  controls.

  <section|TLS certificates><label|sec-certificates>

  TLS (Transport Layer Security) encrypts communication between clients and
  the server, protecting passwords and document content from eavesdropping.
  Using TLS is strongly recommended for any server accessible over a network.

  <subsection|Certificate requirements>

  The server requires two files for TLS:

  <\itemize>
    <item><verbatim|cert.pem>: The server certificate

    <item><verbatim|key.pem>: The private key
  </itemize>

  These files are stored in <verbatim|$TEXMACS_HOME_PATH/server/> (typically
  <verbatim|~/.TeXmacs/server/> for Linux and macOS).

  <subsection|Creating a self-signed certificate>

  <TeXmacs> can generate a self-signed certificate for testing or internal
  use:

  <\enumerate>
    <item>When starting the server without a certificate, a warning dialog
    appears.

    <item>Click <menu|Open Manager> to open the certificate manager.

    <item>Fill in the optional certificate information:

    <\description>
      <item*|Common Name>The hostname or domain name (e.g.,
      <verbatim|myserver.example.com>)

      <item*|Country>Two-letter country code (e.g., <verbatim|US>,
      <verbatim|FR>)

      <item*|City/Locality>City name

      <item*|State>State or province

      <item*|Organization>Organization name

      <item*|Unit>Department or unit within the organization
    </description>

    <item>Click <menu|Create> to generate the certificate.

    <item>If successful, you will be prompted to start the server.
  </enumerate>

  All fields are optional. The certificate will be self-signed, meaning
  clients will need to trust it explicitly.

  <subsection|Using a CA-signed certificate>

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

  <subsection|Renewing certificates>

  To renew an existing certificate, open the certificate manager and create a
  new certificate. The old certificate will be replaced.

  <section|Default administrator account><label|sec-default-admin>

  When the server starts for the first time, an administrator account is
  created:

  <\itemize>
    <item>Username: <verbatim|admin>

    <item>Password: A randomly generated 20-character password
  </itemize>

  The password is displayed in a popup window and logged to the server
  console. <strong|You should change this password immediately> after first
  login using <menu|Remote|Edit account>.

  If you lose the admin password, you can reset it using <menu|Remote|Reset
  admin password> while the server is running locally. This generates a new
  random password and displays it.

  <section|Server preferences>

  Server configuration settings are stored in <TeXmacs> preferences and can
  be modified through the remote preferences form (for administrator users
  connected to the server) or by editing preferences directly. The settings
  are organized into several categories.

  <subsection|Connection settings>

  <\description>
    <item*|Port number>The TCP port the server listens on. Default is
    <verbatim|6561>. Change this if the default port conflicts with other
    services.

    <item*|Contact timeout>Time in milliseconds to wait for initial client
    contact. Default is <verbatim|10000> (10 seconds).

    <item*|Connection timeout>Time in seconds before an idle connection is
    closed. Default is <verbatim|120> (2 minutes).
  </description>

  <subsection|Authentication settings>

  <\description>
    <item*|Anonymous via TLS>When enabled, allows clients to connect
    anonymously over TLS for account creation and login. This should be
    enabled for public servers.
  </description>

  <subsection|Account security>

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

  <subsection|Services>

  These toggles control which services are available to remote clients:

  <\description>
    <item*|Remote password login>Allows users to log in with username and
    password.

    <item*|Remote account creation>Allows new users to create accounts
    remotely. Disable this for invite-only servers.

    <item*|Remote credentials resetting>Allows users to reset their password
    via email. Requires a configured mail command.
  </description>

  <subsection|Password settings>

  <\description>
    <item*|Password encoding>The algorithm used to hash passwords. Options
    include <verbatim|clear> (not recommended), <verbatim|sha256>,
    <verbatim|sha512>, and <verbatim|pbkdf2> (most secure, requires GnuTLS).

    <item*|Require strong passwords>When enabled, passwords must contain at
    least 10 characters including uppercase, lowercase, digit, and symbol.

    <item*|Automatic encoding update>When enabled, passwords are re-encrypted
    with the current encoding algorithm upon successful login.
  </description>

  <subsection|Resetting preferences>

  To reset all server preferences to their defaults, use <menu|Remote|Reset
  preferences> while the server is running locally.

  <section|Server administration><label|sec-remote-admin>

  Server administration is performed through the <TeXmacs> interface by
  connecting to the server as an admin user. This works regardless of whether
  the server runs locally, as a separate process on the same machine, or on a
  remote machine.

  <subsection|Identifying admin connections>

  When connected to a server as an administrator, the cloud icon changes to
  <icon|tm_cloud_admin.xpm> to indicate admin status. Additional menu options
  become available in the <menu|Remote> menu under the <samp|Server
  administration> group.

  <subsection|Editing server preferences>

  Admin users can modify server preferences through the <TeXmacs> interface:

  <\enumerate>
    <item>Connect to the server and log in with an admin account.

    <item>Select <menu|Remote|Edit Server Preferences>.

    <item>A preferences form opens allowing you to modify server settings.

    <item>Changes take effect after submitting the form.
  </enumerate>

  The preferences form includes all configurable options: connection
  settings, authentication, account security, services, password settings,
  email templates, and the server license.

  <subsection|User management>

  Administror users can manage user accounts through the <TeXmacs> interface:

  <\enumerate>
    <item>Connect to the server and log in with an admin account.

    <item>Select <menu|Remote|User Management>.

    <item>The accounts list shows all registered users.

    <item>Select a user and click <menu|Edit> to modify their account.

    <item>Click <menu|Add> to create a new account.
  </enumerate>

  <subsubsection|Creating accounts>

  When creating a new user account, fill in the required information:

  <\description>
    <item*|Pseudo>The username (must be unique)

    <item*|Full name>The user's display name

    <item*|Email>The user's email address

    <item*|Administrator>Toggle to grant admin privileges

    <item*|Password>The initial password
  </description>

  <subsubsection|Editing accounts>

  When editing an existing account, you can modify:

  <\itemize>
    <item>Full name and email

    <item>Administrator status

    <item>Suspended status (to temporarily disable the account)

    <item>Password (change, add, or delete)
  </itemize>

  <subsubsection|Suspending accounts>

  Suspended accounts cannot log in to the server. To suspend an account,
  enable the <menu|Suspended> toggle in the account editor. To resume a
  suspended account, disable the toggle.

  <section|Email configuration>

  The server can send emails for account confirmation and password reset.
  This requires configuring a mail command.

  <subsection|Mail command>

  The mail command is a shell command that sends an email. It receives the
  message content via a temporary file. Available placeholders:

  <\description>
    <item*|$USER_PSEUDO>The user pseudo\ 

    <item*|$USER_NAME>The user full name

    <item*|$USER_EMAIL>The user email address

    <item*|$USER_CODE>The confirmation or reset code

    <item*|$FILE_NAME>Path to the temporary file containing the message
  </description>

  Examples:

  <\verbatim-code>
    sendmail $USER_EMAIL \< $FILE_NAME
  </verbatim-code>

  or

  <\verbatim-code>
    cat $FILE_NAME \| mail $USER_EMAIL -s \\"TeXmacs server Account
    information\\" -r 'reply-to@mydomain.com'
  </verbatim-code>

  <subsection|Email templates>

  The server uses two email templates stored in
  <verbatim|$TEXMACS_HOME_PATH/server/>:

  <\description>
    <item*|email-new-account.txt>Sent when a new account requires
    confirmation.

    <item*|email-reset-credentials.txt>Sent when a user requests a password
    reset.
  </description>

  Templates support the same placeholders as the mail command. Default
  templates are copied from the <TeXmacs> installation on first use.
  Administrator users can edit these templates through the remote preference
  form.

  <section|Server license>

  You can configure a license agreement that users must accept when creating
  an account. The license is stored as a <TeXmacs> document in
  <verbatim|$TEXMACS_HOME_PATH/server/license.tm>.

  To edit the license:

  <\enumerate>
    <item>Connect to the server as an admin user.

    <item>Open <menu|Remote|Edit Server Preferences>.

    <item>Edit the license section in the preferences form.

    <item>Submit the changes.
  </enumerate>

  The license content is displayed to users during account creation.

  <section|Logging>

  The server logs events at different severity levels:

  <\description>
    <item*|Emergency, Alert, Critical, Error>Serious issues requiring
    attention.

    <item*|Warning>Important notices such as password resets.

    <item*|Notice, Info, Debug>Informational messages for troubleshooting.
  </description>

  Server logs are reported to the correct logging facility depending on the
  OS the server is running.

  More verbose debugging information can be activated with the following
  command line flags:

  <verbatim|--debug-gnutls --debug-io --debug-remote --debug-sockets>

  Which are also available in the Debugging tool menu in the <TeXmacs>
  interface.

  <section|Security tips>

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

  <section|Server data location>

  All server data is stored in <verbatim|$TEXMACS_HOME_PATH/server/>,
  typically <verbatim|~/.TeXmacs/server/>:

  <\description>
    <item*|cert.pem, key.pem>TLS certificate and private key.

    <item*|users.scm>User account information.

    <item*|pending-users.scm>Accounts awaiting email confirmation.

    <item*|reset-credentials-users.scm>Pending password reset requests.

    <item*|license.tm>Server license agreement.

    <item*|email-new-account.txt>Email template for new accounts.

    <item*|email-reset-credentials.txt>Email template for password resets.
  </description>

  The server database (containing files, chat messages, and other resources)
  is stored in the global <TeXmacs> database.

  <section|Troubleshooting>

  <\description>
    <item*|Server fails to start>Check that the configured port is not
    already in use. Wait for a previous process releases the port, or use a
    different port number.

    <item*|TLS errors>Verify that the certificate and key files exist and are
    valid. Try regenerating the certificate.

    <item*|Users cannot connect or create an account>Ensure the firewall
    allows connections on the server port. Verify that the server address is
    reachable from the client. Verify the remote login and/or remote account
    creation are activated

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