<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Revisión ortográfica>

  Si el programa <verbatim|ispell> ha sido instalado en su sistema, entonces
  puede usarlo para hacer la revisión ortográfica de sus documentos, pulsando
  <shortcut|(spell-start)> o mediante <apply|menu|Edit|Spell>. Esto actuará sobre
  el texto completo o sobre la región seleccionada, si la tienes. Observe que
  debes verificar que el diccionario de español (o del idioma en que escriba)
  está instalado en su sistema. El inglés suele venir por defecto.

  Cada vez que la revisión ortográfica se encuentre una parabra de ortografía
  dudosa aparecerán una serie de opciones en la parte inferior de la ventana

  <\description>
    <expand|item*|a)>Aceptar la palabra incorrecta, así como todas sus
    apariciones futuras en el ámbito de la revisión.

    <expand|item*|r)>Sustituir la palabra culpable por una corrección (que
    tienes que introducir).

    <expand|item*|i)>Indicar que la palabra ``incorrecta'' es en realidad
    correcta, y que quieres que se inserte en tu diccionario personal.

    <expand|item*|1-9)>Diversas sugerencias de correción.
  </description>

  Observe que <verbatim|ispell> sólo busca palabras ortográficamente
  incorrectas. No hay corrección gramatical.

  Tenga en cuenta que, dado que el corrector ortográfico arranca en el idioma
  activo en la posición del cursor (o al inicio de una selección), sólo se va
  a revisar el texto en ese idioma. Si el documento es multilingüe, tendrá
  que arrancar una vez el corrector ortográfico por cada idioma que use.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
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
    <associate|toc-1|<tuple|0.<error|bad plus>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Ortografía>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
