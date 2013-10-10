<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Usar el estilo tmdoc>

  Además de los macros de <hlink|información de copyright|copyright.es.tm> y
  <hlink|macros transversales|traversal.es.tm>, los cuales han sido
  docuementados antes, el estilo <tmstyle|tmdoc> viene con cierto número de
  otros macros y funciones, las cuales usted debe usar cuando sea apropiado:

  <\explain|<markup|key>>
    Este macro es usado para indicar una entrada de teclado como
    <shortcut|(save-buffer)>. Los macros especializados <markup|kbd-gen>,
    <markup|kbd-text>, <markup|kbd-math>, <markup|kbd-symb>,
    <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>, <markup|kbd-exec>
    y <markup|kbd-table> son usados para entradas de teclado correspondientes
    a tipos específicos de acción o modos. Por ejemplo, <markup|kbd-math>
    corresponde a atajos de teclado para operaciones matemáticas, tales como
    <key|math f>, el cual inicia una fracción.
  </explain>

  <\explain|<markup|menu>>
    Esta función con un número arbitrario de argumentos indica un menú como
    <menu|File> o <menu|Document|Language>. Las entradas de menu con
    automáticamente traducidas por esta función.
  </explain>

  <\explain|<markup|markup>>
    Esta macro es usado a fin de indicar un macro o una función como
    <markup|section>.
  </explain>

  <\explain|<markup|tmstyle>>
    Este macro indica el nombre de un archivo estilo <TeXmacs> o un paquete
    como <tmstyle|article>.
  </explain>

  <\explain|<markup|tmpackage>>
    Este macro indica el nombre de un paquete <TeXmacs> como
    <tmpackage|std-markup>.
  </explain>

  <\explain|<markup|tmdtd>>
    Este macro indica de un <abbr|d.t.d.> de <TeXmacs> like
    <tmdtd|number-env>.
  </explain>

  Note que los contenidos de ninguna de las etiquetas de arriba deben ser
  traducidos a lenguajes extrangeros. De hecho, para etiquetas de menú, las
  traducciones son hechas automáticamente, así se mantienen sincronizadas con
  las traducciones en los menus acutales de <TeXmacs>. En los casos de
  etiquetas, estilos, paquetes y <abbr|d.t.d.>s, es importante mantener el
  nombre original, porque a menudo corresponde a un nombre de archivo.

  Los siguientes macros y funciones son usadas para propósitos de enlace e
  indexación, aunque ellos deben ser mejorados en el futuro:

  <\explain|<markup|simple-link>>
    Este macro toma una URL <math|x> como argumento y es un hiperenlace con
    un nombre y destinación <math|x>.
  </explain>

  <\explain|<markup|hyper-link>>
    Este macro is un hiperenlace usual.
  </explain>

  <\explain|<markup|concept-link>>
    Este macro toma un concepto como argumento. Después un hiperenlace
    apropriado podría ser creado automaticamente desde esta y la otra
    documentación.
  </explain>

  <\explain|<markup|only-index>>
    Indexa una cadena simple.
  </explain>

  <\explain|<markup|def-index>>
    Definición de un nuevo concepto; el texto es impreso en itálica e
    indexado.
  </explain>

  <\explain|<markup|re-index>>
    Reaparición de un concepto ya definido; el texto es impreso en romano y
    puesto en el índice.
  </explain>

  Las siguientes etiquetas son también frecuentemente usadas:

  <\explain|<markup|icon>>
    Enlace a un icono en un directorio central como
    <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.
  </explain>

  <\explain|<markup|screenshot>>
    Enlace a una captura de pantalla. Las capturas de pantalla actuales son
    almacenadas en un directorio central como
    <verbatim|$TEXMACS_PATH/doc/images/screenshots>.
  </explain>

  <\explain|<markup|scheme>>
    El lenguage <scheme>.
  </explain>

  <\explain|<markup|framed-fragment>>
    Para mostrar un pedazo de código en un marco agradable.
  </explain>

  <\explain|<markup|scheme-fragment>>
    Para código <scheme> multi-párrafo.
  </explain>

  <\explain|<markup|tm-fragment>>
    Para un pedazos de etiquetas de código <TeXmacs> en formato <scheme>.
  </explain>

  <\explain|<markup|descriptive-table>>
    Para tablas descriptivas; tales tablas pueden ser usadas para documentar
    listas de atajos de teclado, diferentes tipos de etiquetas, etc.
  </explain>

  El estilo <tmstyle|tmdoc> herededa de el estilo <tmstyle|generic> y usted
  debe usar macros como <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> para este estilo donde sea apropiado.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|spanish>
  </collection>
</initial>