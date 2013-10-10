<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Etiquetas estándar>

  Varias etiquetas estándar están definidas en <tmdtd|std-markup>. Los
  siguientes etiquetas de contenido textual toman todas un argumento. La
  maryoría puede ser encontradas en el menú <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Indica una <strong|importante> región de texto. Pueden entrar esta
    etiqueta vía <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Enfatiza una región del texto como en ``la cosa <em|real>''. Esta
    etiqueta corresponde a la entrada del menú \ <menu|Insert|Content
    tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Para definiciones como ``un <dfn|gnu> es una bestia cornuda''. Esta
    etiqueta corresponde a <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Una secuencia de caracteres literales como la ligadura <samp|ae> (æ).
    Puede obtener esta etiqueta vía <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    El nombre de una cosa o concepto particular como el sistema <name|Linux>.
    Esta etiqueta es obtenida usando <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<markup|person>>
    El nombre de una persona como <name|Joris>. Esta etiqueta corresponde a
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Una cita bibliográfica particuar como un libro o revista. Ejemplo:
    <cite*|Moby Dick> de Melville. Esta etiqueta, que es obtenida usando
    <menu|Insert|Content tag|Cite>, no debe ser confundida con <markup|cite>.
    La última etiqueta es también usada para citas, pero cuando el artumento
    se refiere a una entrada en una base de datos con referencias
    bibliográficas.
  </explain>

  <\explain|<markup|abbr>>
    Una abreviación. Ejemplo: trabajo en el <abbr|C.N.R.S.> Una abreviación
    es creada usando <menu|Insert|Content tag|Abbreviation> o el atajo de
    teclado <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Un acrónimo es una abreviación formada por la priemra letra de cada
    palabra en un nombre o frase, tal como <acronym|HTML> o <acronym|IBM>. En
    particular, las letras no están separadas por puntos. Puede entrar un
    acrónimo usando <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Texto literal como la salida de un programa de computador. Ejemplo: El
    programa dijo <verbatim|hola>. Puede ingresar texto literal vía
    <menu|Insert|Content tag|Verbatim>. La etiqueta también puede ser usada
    como un entorno para texto multipárrafo.
  </explain>

  <\explain|<markup|kbd>>
    Texto que debería ser ingresado en el teclado. Ejemplo: por favor
    presione <kbd|enter>. Esta etiqueta corresponde a la entrada de menú
    <menu|Insert|Content tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    Código de un programa de computadora como en ``<code*|cout
    \<less\>\<less\> 1+1;> produce <verbatim|2>''. Esto es entrado usando
    <menu|Insert|Content tag|Code>. Para trozos más largos de código, debería
    usar el entorno <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Variables en un programa de computador como en <verbatim|cp
    <var|src-file> <var|dest-file>>. Esta etiqueta corresponde a la entrada
    del menú <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Esta es una etiqueta que será usada en el futuro para texto matemático
    dentro del texto regular. Ejemplo: la fórmula <math|sin<rsup|2>
    x+cos<rsup|2> x=1> es bien-conocida.
  </explain>

  <\explain|<markup|op>>
    Esta es una etiqueta que puede ser usada dentro del entorno matemático
    para especificar que un operador puede ser considerado en sí mismo, sin
    algún otro argumento. Ejemplo: la operación <math|<op|+>> es una función
    de <math|\<bbb-R\><rsup|2>> a <math|\<bbb-R\>>. Esta etiqueta puede
    entrar en desuso.
  </explain>

  <\explain|<markup|tt>>
    Este es una etiqueta física para una frase con texto de máquina de
    escribir. Es usado por compabilidad con <name|HTML>, pero no recomendamos
    su uso.
  </explain>

  Los siguientes son ambientes estándar:

  <\explain|<markup|verbatim>>
    Descrito arriba.
  </explain>

  <\explain|<markup|code>>
    Similar a <markup|code*>, pero para piezas de código de varias líneas.
  </explain>

  <\explain|<markup|quote>>
    Entorno para citaciones cortas (de un párrafo).
  </explain>

  <\explain|<markup|quotation>>
    Entorno para citaciones largas. (multi-párrafo).
  </explain>

  <\explain|<markup|verse>>
    Entorno para poesía.
  </explain>

  <\explain|<markup|center>>
    Esta es una etiqueta física para centrar una o varias líneas de texto. Es
    usado para compatibilidad con <name|HTML>, pero no recomendamos su uso.
  </explain>

  Algunos ambientes tabulares estándar son:

  <\explain|<markup|tabular*>>
    Tablas centradas.
  </explain>

  <\explain|<markup|block>>
    Tablas alineadas a la izquierda con un borde estándar de <verbatim|1ln>
    de ancho.
  </explain>

  <\explain|<markup|block*>>
    Tablas centradas con un borde estándar de <verbatim|1ln> de ancho.
  </explain>

  Las siguientes etiquetas misceláneas no tomas argumentos:

  <\explain|<markup|TeXmacs>>
    El logo de <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    El logo de <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    El logo de <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Usado por los desarrolladores para nivelar a la derecha en la definición
    de entornos.
  </explain>

  <\explain|<markup|hrule>>
    Una regla horizontal como la que ve abajo:

    <hrule>
  </explain>

  Las siguientes etiquetas misceláneas toman todas uno o más argumentos:

  <\explain|<markup|overline>>
    Para <overline|texto con una supralínea>, que puede ser cubierto a lo
    largo de varias líneas.
  </explain>

  <\explain|<markup|underline>>
    Para <underline|texto subrayado>, que puede ser cubierto a lo largo de
    varias líneas..
  </explain>

  <\explain|<markup|fold>>
    Macro con dos argumentos. El primer argumento es mostrado y el segundo es
    ignorado: El macro corresponde a la presentación plegada de una pieza de
    contenido asociada a un corto título o resumen. El segundo argumento
    puede ser hecho visible usando <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|unfold>>
    Macro con dos argumentos <var|x> y <var|y>, que produce la presentación
    desplegada de una pieza de contenidos <var|y> asociado a un pequeño
    título o resumen <var|x>. El segundo argumento puede ser hecho visible
    usando <menu|Insert|Switch|Fold>.
  </explain>

  <\explain|<markup|switch>>
    Macro con dos argumentos <var|x> y <var|y>, donde <var|y> es un conjunto
    de posibles representaciones del intercambio y <var|x> la representación
    actual. Las teclas de función <key|F9>, <key|F10>, <key|F11> y <key|F12>
    pueden ser usadas para intercambiar entre diferentes representaciones.
  </explain>

  <\explain|<markup|phantom>>
    Función con un argumento <var|x>. Esta etiqueta toma tanto espacio como
    la composición tipográfica del argumento <var|x> tomaría, pero <var|x> no
    es mostrado. Por ejemplo, el texto ``fantasma'' como un argumento de
    <markup|phantom> produce ``<phantom|fantasma>''.
  </explain>

  <\explain|<markup|set-header>>
    Función con un argumento para cambiar permanentemente la cabecera. Note
    que ciertas etiquetas en el archivo de estilo, como las etiquetas de
    sección, pueden sobreescribir tales cambios manuales.
  </explain>

  <\explain|<markup|set-footer>>
    Función con un argumento para cambiar permanentemente el pie.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|El permiso está garantizado para copiar, distribuir y/o
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