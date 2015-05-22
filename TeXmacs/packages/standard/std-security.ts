<TeXmacs|1.99.2>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-security|1.0>

    <\src-purpose>
      Macros for encrypting pieces of text
    </src-purpose>

    <src-copyright|2015|Grégoire Lecerf>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Public key encryption via GnuPG.
    </src-comment>
  </active*>

  <assign|gpg-info-level|detailed>

  <assign|gpg-render-recipient|<\macro|z>
    <use-module|(security gpg gpg-edit)><extern|tm-gpg-get-key-user-id|<arg|z>>
  </macro>>

  <assign|gpg-render-recipient-item|<\macro|z>
    <htab|3em>\U <gpg-render-recipient|<arg|z>>
  </macro>>

  <assign|gpg-decrypted-block-none|<\macro|body|recipients>
    <\surround|<no-indent><flag|Decrypted|dark green>|<right-flush>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-decrypted-block-short|<\macro|body|recipients>
    <\surround|<no-indent><framed|<with|font-series|bold|Decrypted
    block>><new-line><new-line>|<right-flush><new-line><hrule>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-decrypted-block-detailed|<\macro|body|recipients>
    <\surround|<no-indent><framed|<with|font-series|bold|Decrypted block with
    recipients><new-line><arg|recipients><new-line>><new-line><new-line>|<right-flush><new-line><hrule>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-decrypted-block|<\xmacro|v>
    <compound|<merge|gpg-decrypted-block-|<value|gpg-info-level>>|<arg|v|0>|<map-args|gpg-render-recipient-item|document|v|1>>
  </xmacro>>

  <assign|gpg-encrypted-block|<\xmacro|v>
    <\surround|<no-indent>|>
      <\framed>
        <with|font-series|bold|Encrypted block with
        recipients><new-line><map-args|gpg-render-recipient-item|document|v|1>
      </framed>
    </surround>
  </xmacro>>

  <assign|gpg-decrypted|<xmacro|v|<arg|v|0>>>

  <assign|gpg-encrypted|<xmacro|v|<inline-tag|encrypted-data>>>

  <\active*>
    <\src-comment>
      Passphrase encryption via GnuPG.
    </src-comment>
  </active*>

  <assign|gpg-passphrase-decrypted-block-none|<\macro|body>
    <\surround|<no-indent><flag|Passphrase decrypted|dark
    green>|<right-flush>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-passphrase-decrypted-block-short|<\macro|body>
    <\surround|<no-indent><framed|<with|font-series|bold|Passphrase decrypted
    block>><new-line><new-line>|<right-flush><new-line><hrule>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-passphrase-decrypted-block-detailed|<\macro|body>
    <\surround|<no-indent><framed|<with|font-series|bold|Passphrase decrypted
    block>><new-line><new-line>|<right-flush><new-line><hrule>>
      <arg|body>
    </surround>
  </macro>>

  <assign|gpg-passphrase-decrypted-block|<macro|body|<compound|<merge|gpg-passphrase-decrypted-block-|<value|gpg-info-level>>|<arg|body>>>>

  <assign|gpg-passphrase-encrypted-block|<\macro|body>
    <\surround|<no-indent>|>
      <\framed>
        <with|font-series|bold|Passphrase encrypted block>
      </framed>
    </surround>
  </macro>>

  <assign|gpg-passphrase-decrypted|<macro|v|<arg|v>>>

  <assign|gpg-passphrase-encrypted|<macro|v|<inline-tag|passphrase-encrypted-data>>>

  <\active*>
    <\src-comment>
      Passphrase encryption of whole buffer via GnuPG.
    </src-comment>
  </active*>

  <assign|gpg-passphrase-encrypted-buffer|<\macro|body>
    <\surround|<no-indent>|>
      <\framed>
        <with|font-series|bold|Passphrase encrypted buffer>
      </framed>
    </surround>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>