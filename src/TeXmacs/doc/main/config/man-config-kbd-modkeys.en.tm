<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configuration of the modifier keys>

  <apply|TeXmacs> uses five major keyboard modifiers:
  <key|<expand|key-shift>>, <key|<expand|key-control>>,
  <key|<expand|key-alternate>>, <key|<expand|key-meta>> and
  <key|<expand|key-hyper>>, which are abbreviated as <key|S->, <key|C->,
  <key|A->, <key|M-> and <key|H->. The <key|<expand|key-shift>> and
  <key|<expand|key-control>> keys are present on virtually all keyboards and
  the <key|<expand|key-alternate>> key on almost all. Most keyboards for PC's
  nowadays also have a <key|<key-windows>> key, which is usually
  equivalent to <key|<expand|key-meta>> for <TeXmacs>.

  Before reconfiguring your keyboard, you should first check that this is
  indeed necessary. If you have keys which correspond to
  <key|<expand|key-shift>>, <key|<expand|key-control>>,
  <key|<expand|key-alternate>> and <key|<expand|key-meta>> in a suitable way,
  then you probably do not want to do anything. A possible exception is when
  you want to use a simple key like <key|<key-caps-lock>> for
  typing mathematical symbols. In that case, you should map
  <key|<key-caps-lock>> to <key|<expand|key-hyper>>.

  In order to reconfigure the keyboard, you simply select the logicial
  modifier that you want to correspond to a given physical key in
  <apply|menu|Edit|Preferences|Keyboard>. For instance, selecting
  <apply|menu|Windows key|Map to M modifier>, the
  <key|<key-windows>> key will correspond to the
  <key|<expand|key-meta>> modifier. Similarly, when selecting
  <apply|menu|Caps-lock key|Map to H modifier>, the
  <key|<key-caps-lock>> key will correspond to the
  <key|<expand|key-hyper>> modifier.

  Unfortunately, X Window only allows system-wide reconfiguration.
  Consequently, if you reconfigure the <key|<key-caps-lock>> key
  inside <apply|TeXmacs>, then the new behaviour of
  <key|<key-caps-lock>> will affect all other applications too. It
  is therefore important to reconfigure only those keys which you do not use
  for something else in other applications. For instance, the
  <key|<key-windows>> key is not used by many applications, so it
  generally does not do any harm to reconfigure it. You may also prefer to
  perform an appropriate system-wide configuration. This can be done using
  the <verbatim|xmodmap> command; see the corresponding manual page for more
  information.

  In certain cases, you already have keys on your keyboard which correspond
  to <key|<expand|key-alternate>>, <key|<expand|key-meta>> and
  <key|<expand|key-hyper>>, but not in the way you want. This can be done by
  remapping the <key|A->, <key|M-> and <key|H-> prefixes to other logical
  modifiers in the first group of submenus of
  <apply|menu|Edit|Preferences|Keyboard>.

  For instance, for Emacs compatability, you might want to permute the
  <key|<expand|key-meta>> or <key|<key-windows>> key with
  <key|<expand|key-alternate>> without making any system-wide changes. This
  can be done by finding out which modifiers correspond to these keys;
  usually this will be <key|Mod1> for <key|<expand|key-alternate>> and
  <key|Mod4> for <key|<expand|key-meta>> or <key|<key-windows>>.
  We next perform the necessary permutation in
  <apply|menu|Edit|Preferences|Keyboard>, by selecting <apply|menu|A
  modifier|Equivalent for Mod4> and <apply|menu|M modifier|Equivalent for
  Mod1>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Preferences>|<with|font
      family|<quote|ss>|Keyboard>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Windows key>|<with|font
      family|<quote|ss>|Map to M modifier>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Caps-lock key>|<with|font
      family|<quote|ss>|Map to H modifier>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Preferences>|<with|font
      family|<quote|ss>|Keyboard>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Preferences>|<with|font
      family|<quote|ss>|Keyboard>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|A modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|M modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
