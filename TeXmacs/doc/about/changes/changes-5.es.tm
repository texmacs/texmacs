<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Teclado (1.0.0.11 -- 1.0.1)>

  Los atajos de teclado de <apply|TeXmacs> han sido racionalizados. A
  continuación sigue una lista de los cambios principales:

  <\itemize>
    <item>El <key|E-> prefijo ha sido renombrado a <prefix|M->.

    <item><key|escape> es equivalente a <prefix|M-> y <key|escape>-<key|escape>
    a <prefix|A->.

    <item>Comando dependientes del modo están ahora precedidos por <prefix|A->.
    En particular, los acentos son tecleados usando <prefix|A-> en lugar de
    <key|E->.

    <item>Las variantes son ahora obtenidas usando <key|tab> en lugar de
    <key|*> y puede recorrerlas al revés usando
    <key|S-tab>.

    <item>Los caracteres griegos son ahora tecleados usando <prefix|A-C->,
    <prefix|math:greek>, o el modificador hiper, el cual puede ser configurado en
    <apply|menu|Edit|Preferences>. También puede obtener caracteres griegos
    como variantes de caracteres latinos. Por ejemplo, <key|p tab> produce
    <with|mode|math|\<pi\>>.

    <item>La significación de las teclas del cursor en combinación con lata
    teclas control, alt y meta ha cambiado.
  </itemize>

  Puede ahora escoger entre varios ``aspectos y comportamientos'' para el
  comportamiento del teclado en<samp| ><apply|menu|Edit|Preferences|look and
  feel>. Por defecto está en <samp|<apply|menu|Emacs>>, pero usted puede
  esciger <samp|<apply|menu|Old style>> si usted quiere mantener el
  comportamiento al que puede estar acostumbrado.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font family|<quote|ss>|aspecto y
      comportamiento>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Emacs>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Estilo
      antiguo>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
