<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating a self-signed certificate>

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

  <paragraph|Renewing Certificates>

  To renew an existing certificate, open the certificate manager and create a
  new certificate. The old certificate will be replaced.

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