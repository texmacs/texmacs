<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configuración de las teclar modificadoras>

  <apply|TeXmacs> usa cinco modificadores de teclado principales:
  <prefix|S->, <key|control>, <key|alternate>, <key|meta> e <key|hyper>, que
  son abreviadas como <prefix|S->, <prefix|C->, <prefix|A->, <prefix|M-> and <prefix|M-A->.
  Las teclas <prefix|S-> y <key|control> están presentes en virtualmente
  todos los teclados y la tecla <key|alternate> en la mayoría. Muchos
  fabricantes de PC's estos días tienen también una tecla \ <key|windows>, la
  cual es usualmente equivalente a la tecla <key|meta> para <TeXmacs>.

  Antes de reconfigurar su teclado, debería primero chequear que esto es de
  hecho necesario. Si tiene teclas que corresponden a <prefix|S->,
  <key|control>, <key|alternate> y <key|meta> en una forma apropidad,
  entonces problamente no requiera hacer algo. Una posible excepción es
  cuando usted quiera sar una tecla simple como <key|capslock> para teclear
  símbolos matemáticos. En ese caso, debería hacer corresponder
  <key|capslock> a <key|hyper>.

  A fin de reconfigurar el teclado, simplemente selccione el moficador lógico
  que quiere corresponder a una tecla física dada en
  <apply|menu|Edit|Preferences|Keyboard>. Por ejemplo, seleccionado
  <apply|menu|Windows key|Map to M modifier>, la tecla <key|windows>
  corresponderá al modificador \ <key|meta>. Similarmente, cuanco seleccione
  <apply|menu|Caps-lock key|Map to H modifier>, la tecla <key|capslock>
  corresponderá al modificador <key|hyper>.

  Infortunadamente, X Window sólo permite una reconfiguración a lo ancho del
  sistema. Consecuentemente, si usted reconfigura la tecla <key|capslock>
  dentro de <apply|TeXmacs>, entonces el nuevo comportamiento de
  <key|capslock> afectará todas las otras aplicaciones también. Es por esto
  importante reconfigurar sólo aquellas teclas que usted no usa para algo más
  en otras aplicaciones. Por ejemplo, la tecla <key|windows> no es usada por
  muchas aplicaciones, así que generamente no hace ningún daño
  reconfigurarla. Puede también preferir realizar alguna configuración a lo
  ancho del sistema. Esto puede ser hecho con el comando <verbatim|xmodmap>;
  vea la página correspondiente del manual para mayor información.

  En ciertos casos, usted ya tiene teclas en su teclado que corresponde a
  <key|alter>, <key|meta> e <key|hyper>, pero no en la forma en que usted
  quiere. Esto puede ser hecho reasinando los prefijos \ <prefix|A->, <prefix|M-> y
  <prefix|M-A-> a otros modificadores lógicos en el primer grupo de submenus de
  <apply|menu|Edit|Preferences|Keyboard>.

  Por ejemplo, para compatibilidad con <name|Emacs>, podría querer permutar
  la tecla <key|meta> o <key|windows> con <key|alter> sin hacer ningún cambio
  a lo amplio de sistema. Esto puede ser hecho encontrando que modificadores
  corresponde a estas teclas; usualmente esto será <key|Mod1> para
  <key|alter> y <key|Mod4> para <key|meta> o <key|windows>. Realizaremos las
  permutaciones necesarias en <apply|menu|Edit|Preferences|Keyboard>,
  selecionando <apply|menu|A modifier|Equivalent for Mod4> y <apply|menu|M
  modifier|Equivalent for Mod1>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

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
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tecla de Windows>|<with|font
      family|<quote|ss>|Map to M modifier>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Caps-lock key>|<with|font
      family|<quote|ss>|Map to H modifier>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|A modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|M modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
