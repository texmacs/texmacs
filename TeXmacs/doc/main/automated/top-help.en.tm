<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate>>

<\body>
  <block-assign|<implied-scm|styles>|<implied-scm|(get-style-list)>>

  <block-assign|<implied-scm|style>|<implied-scm|(if (null? styles) "none"
  (car styles))>>

  <block-assign|<implied-scm|style-doc>|<implied-scm|(tmdoc-search-style
  style)>>

  <tmdoc-title|Top-level contextual help>

  <\unfolded-documentation|Top-level editing>
    The <help-link|current focus|main/text/man-structure> is on the entire
    document. In this particular situation, the focus toolbar displays some
    important information about your document, such as its style
    (<tmstyle|<output-string|<implied-scm|style>>>), paper size
    (<output-string|<implied-scm|(get-init "page-type")>>) and font size
    (<output-string|<implied-scm|(get-init "font-base-size")>>pt). The
    toolbar also indicates the current section at the position of your
    cursor.

    The above properties and the current section can also be changed using
    the items on the focus toolbar or in the <menu|Focus> menu. For instance,
    by clicking on the current style, paper size or font size, a pulldown
    menu will open from which you can modify the current setting. By clicking
    on the <icon|tm_add.xpm> icon after the document style, you may select
    additional style packages.

    Similarly, when clicking on the current section, a pulldown menu with all
    sections in the document will open, which allows you to quickly jump to a
    particular section. In the case when your cursor is at the start of your
    document, and no document title has been entered yet, then
    a<nbsp><menu|Title> button will appear for inserting a title. Similarly,
    if your cursor is just after the title, then an <menu|Abstract> button
    will appear for entering the abstract.
  </unfolded-documentation>

  <\unfolded-documentation|Current document style>
    <\block-if-else|<implied-scm|style-doc>>
      <block-output|<implied-scm|style-doc>>
    <|block-if-else>
      <\explain|<tmstyle|<output-string|<implied-scm|style>>>>
        No documentation available.
      </explain>
    </block-if-else>
  </unfolded-documentation>

  <\block-if|<implied-scm|(and (nnull? styles) (nnull? (cdr styles)))>>
    <\unfolded-documentation|Other active style customizations>
      <\block-for|<implied-scm|pack>|<implied-scm|(cdr styles)>>
        <block-assign|<implied-scm|pack-doc>|<\implied-scm>
          (tmdoc-search-style pack)
        </implied-scm>>

        <\block-if-else|<implied-scm|pack-doc>>
          <block-output|<implied-scm|pack-doc>>
        <|block-if-else>
          <\explain|<tmpackage|<output-string|<implied-scm|pack>>>>
            No documentation available.
          </explain>
        </block-if-else>
      </block-for>
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>