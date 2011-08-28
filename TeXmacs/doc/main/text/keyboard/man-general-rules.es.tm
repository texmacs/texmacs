<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Reglas generales de prefijos>

  Como hay muchos atajos de teclado, es importante tener algunas formas de
  clasificarlas en varias categorías, a fin de hacer más fácil memorizarlas.
  Como regla general, los atajos de teclado que caen en la misma categoría
  son identificados por un prefijo común. Las principales de tales prefijos
  son:

  <\description>
    <expand|item*|<prefix|C->>Los atajos de teclado basados en
    la tecla Control son usados para comandos de edición frecuentemente
    usados. Ellos dependen mucho del ``aspecto y comportamiento'' en
    <apply|menu|Edit|Preferences>. Por ejemplo, si usted usa un aspecto y
    comportamiento comatible con <name|Emacs>, entonces los atajos de la
    forma <prefix|C-> corresponden a los comandos
    <name|Emacs>, como <key|C-y> para pegar texto.

    <expand|item*|<prefix|A->>La tecla alternate key es usada
    para comandos que dependen del modo en el cual usted está. Por ejemplo,
    <key|text s> produce texto <strong|resaltado> en el modo texto y
    una raíz cuadrada en <with|mode|math|<sqrt|>> en el modo matemático. Note
    que <key|escape escape> es equivalente a <prefix|A->.

    <expand|item*|<prefix|M->>La tecla meta es usada para
    comandos <apply|TeXmacs> de propósito general, que pueden ser usados en
    todos los modos. Por ejemplo, <shortcut|(make-label)> produces una etiqueta.
    Es también usado para comandos de edición adicional, como <key|A-w> para
    copiar texto si usted usa el aspecto y comportamiento <name|Emacs>. Note
    que <key|escape> es equivalente a <prefix|M->.

    <expand|item*|<prefix|M-A->>El modificador de teclado de
    usuario es usado para producer símbolos especiales como los catacteres
    griegos en el modo matemático. Puede configurar su teclado para dejar que
    la tecla de mayúsculas juegue el rol de la tecla hiper. El <prefix|math:greek> is
    equivalent to <prefix|M->.
  </description>

  Recordamos que los modificadores particulares que son usados a fin de
  obtener los prefijos <prefix|M-> y <prefix|M-A-> pueden ser
  <apply|hyper-link|configurados|../../config/man-config-kbd-modkeys.es.tm>
  en <apply|menu|Edit|Preferences>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
