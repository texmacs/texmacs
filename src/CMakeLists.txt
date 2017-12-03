# set some variables containing source file lists and project include dirs (excluding external libraries)

### --------------------------------------------------------------------
### the include dirs
### --------------------------------------------------------------------

set (TeXmacs_Include_Dirs
    ./Data/Convert
    ./Data/Document
    ./Data/Drd
    ./Data/History
    ./Data/Observers
    ./Data/String
    ./Data/Tree
    ./Edit
    ./Edit/Editor
    ./Edit/Interface
    ./Edit/Modify
    ./Edit/Process
    ./Edit/Replace
    ./Graphics/Bitmap_fonts
    ./Graphics/Colors
    ./Graphics/Fonts
    ./Graphics/Gui
    ./Graphics/Handwriting
    ./Graphics/Mathematics
    ./Graphics/Pictures
    ./Graphics/Renderer
    ./Graphics/Spacial
    ./Graphics/Types
    ./Kernel/Abstractions
    ./Kernel/Containers
    ./Kernel/Types
    ./Plugins
    ./Plugins/Pdf/LibAesgm
    ./Scheme
    ./Scheme/Guile
    ./Scheme/Scheme
    ./System
    ./System/Boot
    ./System/Classes
    ./System/Files
    ./System/Language
    ./System/Link
    ./System/Misc
    ./Texmacs
    ./Texmacs/Data
    ./Typeset
    ./Typeset/Bridge
    ./Typeset/Concat
    ./Typeset/Page
    ${TEXMACS_SOURCE_DIR}/TeXmacs/include 
    ${TEXMACS_BINARY_DIR}/src/System/ 
)

set (CMAKE_CXX_FLAGS "-Wno-deprecated-register -include config.h")



### --------------------------------------------------------------------
### the main sources
### --------------------------------------------------------------------


set (TeXmacs_Base_SRCS
    ./Data/Convert/BibTeX/conservative_bib.cpp
    ./Data/Convert/BibTeX/parsebib.cpp
    ./Data/Convert/Coq/parse_vernac.cpp
    ./Data/Convert/Generic/generic.cpp
    ./Data/Convert/Generic/indexation.cpp
    ./Data/Convert/Generic/input.cpp
    ./Data/Convert/Generic/post_convert.cpp
    ./Data/Convert/Scheme/from_scheme.cpp
    ./Data/Convert/Scheme/to_scheme.cpp
    ./Data/Convert/Tex/conservative_fromtex.cpp
    ./Data/Convert/Tex/conservative_totex.cpp
    ./Data/Convert/Tex/fromcls.cpp
    ./Data/Convert/Tex/fromtex.cpp
    ./Data/Convert/Tex/fromtex_post.cpp
    ./Data/Convert/Tex/inittex.cpp
    ./Data/Convert/Tex/latex_recover.cpp
    ./Data/Convert/Tex/latex_tools.cpp
    ./Data/Convert/Tex/metadata-acm.cpp
    ./Data/Convert/Tex/metadata-ams.cpp
    ./Data/Convert/Tex/metadata-elsevier.cpp
    ./Data/Convert/Tex/metadata-ieee.cpp
    ./Data/Convert/Tex/metadata-revtex.cpp
    ./Data/Convert/Tex/metadata-springer.cpp
    ./Data/Convert/Tex/metadata.cpp
    ./Data/Convert/Tex/parsetex.cpp
    ./Data/Convert/Tex/tracked_fromtex.cpp
    ./Data/Convert/Tex/tracked_totex.cpp
    ./Data/Convert/Texmacs/fromtm.cpp
    ./Data/Convert/Texmacs/rewrite_equation_number.cpp
    ./Data/Convert/Texmacs/totm.cpp
    ./Data/Convert/Texmacs/upgradetm.cpp
    ./Data/Convert/Verbatim/verbatim.cpp
    ./Data/Convert/Xml/fromxml.cpp
    ./Data/Convert/Xml/parsexml.cpp
    ./Data/Document/new_data.cpp
    ./Data/Document/new_document.cpp
    ./Data/Document/new_style.cpp
    ./Data/Drd/drd_info.cpp
    ./Data/Drd/drd_mode.cpp
    ./Data/Drd/drd_std.cpp
    ./Data/Drd/tag_info.cpp
    ./Data/Drd/vars.cpp
    ./Data/History/archiver.cpp
    ./Data/History/commute.cpp
    ./Data/History/patch.cpp
    ./Data/Observers/edit_observer.cpp
    ./Data/Observers/highlight_observer.cpp
    ./Data/Observers/ip_observer.cpp
    ./Data/Observers/link.cpp
    ./Data/Observers/list_observer.cpp
    ./Data/Observers/tree_addendum.cpp
    ./Data/Observers/tree_pointer.cpp
    ./Data/Observers/tree_position.cpp
    ./Data/Observers/undo_observer.cpp
    ./Data/String/analyze.cpp
    ./Data/String/base64.cpp
    ./Data/String/converter.cpp
    ./Data/String/fast_search.cpp
    ./Data/String/universal.cpp
    ./Data/String/wencoding.cpp
    ./Data/Tree/tree_analyze.cpp
    ./Data/Tree/tree_brackets.cpp
    ./Data/Tree/tree_correct.cpp
    ./Data/Tree/tree_cursor.cpp
    ./Data/Tree/tree_modify.cpp
    ./Data/Tree/tree_search.cpp
    ./Data/Tree/tree_select.cpp
    ./Data/Tree/tree_traverse.cpp
    ./Edit/Editor/edit_main.cpp
    ./Edit/Editor/edit_typeset.cpp
    ./Edit/Interface/edit_complete.cpp
    ./Edit/Interface/edit_cursor.cpp
    ./Edit/Interface/edit_footer.cpp
    ./Edit/Interface/edit_graphics.cpp
    ./Edit/Interface/edit_interface.cpp
    ./Edit/Interface/edit_keyboard.cpp
    ./Edit/Interface/edit_mouse.cpp
    ./Edit/Interface/edit_repaint.cpp
    ./Edit/Modify/edit_delete.cpp
    ./Edit/Modify/edit_dynamic.cpp
    ./Edit/Modify/edit_math.cpp
    ./Edit/Modify/edit_modify.cpp
    ./Edit/Modify/edit_table.cpp
    ./Edit/Modify/edit_text.cpp
    ./Edit/Process/edit_process.cpp
    ./Edit/Replace/edit_search.cpp
    ./Edit/Replace/edit_select.cpp
    ./Edit/Replace/edit_spell.cpp
    ./Graphics/Bitmap_fonts/bitmap_font.cpp
    ./Graphics/Bitmap_fonts/glyph.cpp
    ./Graphics/Bitmap_fonts/glyph_analyze.cpp
    ./Graphics/Bitmap_fonts/glyph_effects.cpp
    ./Graphics/Bitmap_fonts/glyph_ops.cpp
    ./Graphics/Bitmap_fonts/glyph_shrink.cpp
    ./Graphics/Bitmap_fonts/glyph_transforms.cpp
    ./Graphics/Bitmap_fonts/glyph_unserif.cpp
    ./Graphics/Colors/colors.cpp
    ./Graphics/Colors/true_color.cpp
    ./Graphics/Fonts/charmap.cpp
    ./Graphics/Fonts/compound_font.cpp
    ./Graphics/Fonts/find_font.cpp
    ./Graphics/Fonts/font.cpp
    ./Graphics/Fonts/font_database.cpp
    ./Graphics/Fonts/font_guess.cpp
    ./Graphics/Fonts/font_protrusion.cpp
    ./Graphics/Fonts/font_select.cpp
    ./Graphics/Fonts/font_translate.cpp
    ./Graphics/Fonts/font_scripts.cpp
    ./Graphics/Fonts/math_font.cpp
    ./Graphics/Fonts/poor_bbb.cpp
    ./Graphics/Fonts/poor_bold.cpp
    ./Graphics/Fonts/poor_distorted.cpp
    ./Graphics/Fonts/poor_extended.cpp
    ./Graphics/Fonts/poor_italic.cpp
    ./Graphics/Fonts/poor_rubber.cpp
    ./Graphics/Fonts/poor_smallcaps.cpp
    ./Graphics/Fonts/poor_stretched.cpp
    ./Graphics/Fonts/smart_font.cpp
    ./Graphics/Fonts/translator.cpp
    ./Graphics/Fonts/virtual_enhance.cpp
    ./Graphics/Fonts/virtual_font.cpp
    ./Graphics/Gui/widget.cpp
    ./Graphics/Handwriting/learn_handwriting.cpp
    ./Graphics/Handwriting/poly_line.cpp
    ./Graphics/Handwriting/recognize_handwriting.cpp
    ./Graphics/Handwriting/smoothen.cpp
    ./Graphics/Mathematics/math_tree.cpp
    ./Graphics/Mathematics/test_math.cpp
    ./Graphics/Pictures/effect.cpp
    ./Graphics/Pictures/picture.cpp
    ./Graphics/Pictures/raster_picture.cpp
    ./Graphics/Pictures/raster_random.cpp
    ./Graphics/Pictures/scalable.cpp
    ./Graphics/Renderer/basic_renderer.cpp
    ./Graphics/Renderer/brush.cpp
    ./Graphics/Renderer/page_type.cpp
    ./Graphics/Renderer/pencil.cpp
    ./Graphics/Renderer/printer.cpp
    ./Graphics/Renderer/renderer.cpp
    ./Graphics/Spacial/enlightened.cpp
    ./Graphics/Spacial/transformed.cpp
    ./Graphics/Spacial/triangulated.cpp
    ./Graphics/Types/curve.cpp
    ./Graphics/Types/equations.cpp
    ./Graphics/Types/frame.cpp
    ./Graphics/Types/grid.cpp
    ./Graphics/Types/point.cpp
    ./Kernel/Abstractions/basic.cpp
    ./Kernel/Abstractions/command.cpp
    ./Kernel/Abstractions/observer.cpp
    ./Kernel/Abstractions/player.cpp
    ./Kernel/Containers/array.cpp
    ./Kernel/Containers/hashfunc.cpp
    ./Kernel/Containers/hashmap.cpp
    ./Kernel/Containers/hashmap_extra.cpp
    ./Kernel/Containers/hashset.cpp
    ./Kernel/Containers/hashtree.cpp
    ./Kernel/Containers/iterator.cpp
    ./Kernel/Containers/list.cpp
    ./Kernel/Containers/rel_hashmap.cpp
    ./Kernel/Types/modification.cpp
    ./Kernel/Types/parse_string.cpp
    ./Kernel/Types/path.cpp
    ./Kernel/Types/rectangles.cpp
    ./Kernel/Types/space.cpp
    ./Kernel/Types/string.cpp
    ./Kernel/Types/tab.cpp
    ./Kernel/Types/tree.cpp
    ./Kernel/Types/tree_label.cpp
    ./Scheme/Guile/guile_tm.cpp
    ./Scheme/Scheme/glue.cpp
    ./Scheme/Scheme/object.cpp
    ./System/Boot/init_texmacs.cpp
    ./System/Boot/init_upgrade.cpp
    ./System/Boot/preferences.cpp
    ./System/Classes/tm_timer.cpp
    ./System/Classes/url.cpp
    ./System/Files/file.cpp
    ./System/Files/image_files.cpp
    ./System/Files/tm_ostream.cpp
    ./System/Files/web_files.cpp
    ./System/Language/cpp_language.cpp
    ./System/Language/dictionary.cpp
    ./System/Language/hyphenate.cpp
    ./System/Language/language.cpp
    ./System/Language/math_language.cpp
    ./System/Language/mathemagix_language.cpp
    ./System/Language/packrat_grammar.cpp
    ./System/Language/packrat_parser.cpp
    ./System/Language/packrat_serializer.cpp
    ./System/Language/python_language.cpp
    ./System/Language/r_language.cpp
    ./System/Language/scheme_language.cpp
    ./System/Language/scala_language.cpp
    ./System/Language/scilab_language.cpp
    ./System/Language/text_language.cpp
    ./System/Language/verb_language.cpp
    ./System/Link/connection.cpp
    ./System/Link/dyn_link.cpp
    ./System/Link/socket_notifier.cpp
    ./System/Link/pipe_link.cpp
    ./System/Link/socket_link.cpp
    ./System/Link/socket_server.cpp
    ./System/Link/texmacs_client.cpp
    ./System/Link/texmacs_server.cpp
    ./System/Link/tm_link.cpp
    ./System/Misc/data_cache.cpp
    ./System/Misc/fast_alloc.cpp
    ./System/Misc/persistent.cpp
    ./System/Misc/stack_trace.cpp
    ./System/Misc/sys_utils.cpp
    ./Texmacs/Data/new_buffer.cpp
    ./Texmacs/Data/new_project.cpp
    ./Texmacs/Data/new_view.cpp
    ./Texmacs/Data/new_window.cpp
    ./Texmacs/Server/tm_config.cpp
    ./Texmacs/Server/tm_debug.cpp
    ./Texmacs/Server/tm_server.cpp
    ./Texmacs/Texmacs/texmacs.cpp
    ./Texmacs/Window/tm_button.cpp
    ./Texmacs/Window/tm_dialogue.cpp
    ./Texmacs/Window/tm_frame.cpp
    ./Texmacs/Window/tm_window.cpp
    ./Typeset/Boxes/Animate/animate_boxes.cpp
    ./Typeset/Boxes/Basic/basic_boxes.cpp
    ./Typeset/Boxes/Basic/boxes.cpp
    ./Typeset/Boxes/Basic/rubber_boxes.cpp
    ./Typeset/Boxes/Basic/stretch_boxes.cpp
    ./Typeset/Boxes/Basic/text_boxes.cpp
    ./Typeset/Boxes/Composite/composite_boxes.cpp
    ./Typeset/Boxes/Composite/concat_boxes.cpp
    ./Typeset/Boxes/Composite/decoration_boxes.cpp
    ./Typeset/Boxes/Composite/math_boxes.cpp
    ./Typeset/Boxes/Composite/misc_boxes.cpp
    ./Typeset/Boxes/Composite/script_boxes.cpp
    ./Typeset/Boxes/Composite/stack_boxes.cpp
    ./Typeset/Boxes/Composite/superpose_boxes.cpp
    ./Typeset/Boxes/Graphics/graphics_boxes.cpp
    ./Typeset/Boxes/Graphics/grid_boxes.cpp
    ./Typeset/Boxes/Modifier/change_boxes.cpp
    ./Typeset/Boxes/Modifier/highlight_boxes.cpp
    ./Typeset/Boxes/Modifier/modifier_boxes.cpp
    ./Typeset/Bridge/bridge.cpp
    ./Typeset/Bridge/bridge_argument.cpp
    ./Typeset/Bridge/bridge_auto.cpp
    ./Typeset/Bridge/bridge_compound.cpp
    ./Typeset/Bridge/bridge_default.cpp
    ./Typeset/Bridge/bridge_docrange.cpp
    ./Typeset/Bridge/bridge_document.cpp
    ./Typeset/Bridge/bridge_eval.cpp
    ./Typeset/Bridge/bridge_expand_as.cpp
    ./Typeset/Bridge/bridge_formatting.cpp
    ./Typeset/Bridge/bridge_gui.cpp
    ./Typeset/Bridge/bridge_hidden.cpp
    ./Typeset/Bridge/bridge_locus.cpp
    ./Typeset/Bridge/bridge_mark.cpp
    ./Typeset/Bridge/bridge_rewrite.cpp
    ./Typeset/Bridge/bridge_surround.cpp
    ./Typeset/Bridge/bridge_with.cpp
    ./Typeset/Bridge/typesetter.cpp
    ./Typeset/Concat/concat_active.cpp
    ./Typeset/Concat/concat_animate.cpp
    ./Typeset/Concat/concat_graphics.cpp
    ./Typeset/Concat/concat_gui.cpp
    ./Typeset/Concat/concat_inactive.cpp
    ./Typeset/Concat/concat_macro.cpp
    ./Typeset/Concat/concat_math.cpp
    ./Typeset/Concat/concat_post.cpp
    ./Typeset/Concat/concat_text.cpp
    ./Typeset/Concat/concater.cpp
    ./Typeset/Env/env.cpp
    ./Typeset/Env/env_animate.cpp
    ./Typeset/Env/env_default.cpp
    ./Typeset/Env/env_exec.cpp
    ./Typeset/Env/env_inactive.cpp
    ./Typeset/Env/env_length.cpp
    ./Typeset/Env/env_semantics.cpp
    ./Typeset/Format/format.cpp
    ./Typeset/Format/formatter.cpp
    ./Typeset/Format/line_item.cpp
    ./Typeset/Format/page_item.cpp
    ./Typeset/Line/lazy_gui.cpp
    ./Typeset/Line/lazy_paragraph.cpp
    ./Typeset/Line/lazy_typeset.cpp
    ./Typeset/Line/lazy_vstream.cpp
    ./Typeset/Line/line_breaker.cpp
    ./Typeset/Page/columns_breaker.cpp
    ./Typeset/Page/new_breaker.cpp
    ./Typeset/Page/make_pages.cpp
    ./Typeset/Page/page_breaker.cpp
    ./Typeset/Page/pager.cpp
    ./Typeset/Page/skeleton.cpp
    ./Typeset/Page/vpenalty.cpp
    ./Typeset/Stack/stacker.cpp
    ./Typeset/Table/cell.cpp
    ./Typeset/Table/table.cpp
)

SET(TeXmacs_Std_Plugins_SRCS
    ./Plugins/Axel/axel.cpp
    ./Plugins/Bibtex/bibtex.cpp
    ./Plugins/Bibtex/bibtex_functions.cpp
    ./Plugins/Cairo/cairo_renderer.cpp
    ./Plugins/Cairo/tm_cairo.cpp
    ./Plugins/Database/database.cpp
    ./Plugins/Database/db_disk.cpp
    ./Plugins/Database/db_index.cpp
    ./Plugins/Database/db_query.cpp
    ./Plugins/Database/db_sort.cpp
    ./Plugins/Freetype/adjust_bonum.cpp
    ./Plugins/Freetype/adjust_pagella.cpp
    ./Plugins/Freetype/adjust_schola.cpp
    ./Plugins/Freetype/adjust_termes.cpp
    ./Plugins/Freetype/adjust_stix.cpp
    ./Plugins/Freetype/free_type.cpp
    ./Plugins/Freetype/rubber_assemble_font.cpp
    ./Plugins/Freetype/rubber_stix_font.cpp
    ./Plugins/Freetype/rubber_unicode_font.cpp
    ./Plugins/Freetype/tt_analyze.cpp
    ./Plugins/Freetype/tt_face.cpp
    ./Plugins/Freetype/tt_file.cpp
    ./Plugins/Freetype/tt_font.cpp
    ./Plugins/Freetype/tt_tools.cpp
    ./Plugins/Freetype/unicode_font.cpp
    ./Plugins/Freetype/unicode_math_font.cpp
    ./Plugins/Ghostscript/ghostscript.cpp
    ./Plugins/Ghostscript/gs_utilities.cpp
    ./Plugins/Imlib2/imlib2.cpp
    ./Plugins/Ispell/ispell.cpp
    ./Plugins/LaTeX_Preview/latex_preview.cpp
    ./Plugins/Metafont/adjust_cmr.cpp
    ./Plugins/Metafont/load_pk.cpp
    ./Plugins/Metafont/load_tex.cpp
    ./Plugins/Metafont/load_tfm.cpp
    ./Plugins/Metafont/tex_files.cpp
    ./Plugins/Metafont/tex_font.cpp
    ./Plugins/Metafont/tex_init.cpp
    ./Plugins/Metafont/tex_rubber_font.cpp
    ./Plugins/Mplayer/mplayer.cpp
    ./Plugins/Openssl/openssl.cpp
    ./Plugins/Pdf/pdf_hummus_renderer.cpp
    ./Plugins/Pdf/LibAesgm/aes_modes.c
    ./Plugins/Pdf/LibAesgm/aes_ni.c
    ./Plugins/Pdf/LibAesgm/aeskey.c
    ./Plugins/Pdf/LibAesgm/aestab.c
    ./Plugins/Pdf/LibAesgm/aescrypt.c
    ./Plugins/Pdf/PDFWriter/DocumentContext.cpp
    ./Plugins/Pdf/PDFWriter/PDFDocumentHandler.cpp
    ./Plugins/Pdf/PDFWriter/PDFParser.cpp
    ./Plugins/Pdf/PDFWriter/CFFFileInput.cpp
    ./Plugins/Pdf/PDFWriter/AbstractContentContext.cpp
    ./Plugins/Pdf/PDFWriter/CFFEmbeddedFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/Type1ToCFFEmbeddedFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/PDFWriter.cpp
    ./Plugins/Pdf/PDFWriter/CharStringType2Interpreter.cpp
    ./Plugins/Pdf/PDFWriter/Type1ToType2Converter.cpp
    ./Plugins/Pdf/PDFWriter/OpenTypeFileInput.cpp
    ./Plugins/Pdf/PDFWriter/ObjectsContext.cpp
    ./Plugins/Pdf/PDFWriter/FreeTypeFaceWrapper.cpp
    ./Plugins/Pdf/PDFWriter/TrueTypeEmbeddedFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/Type1Input.cpp
    ./Plugins/Pdf/PDFWriter/JPEGImageHandler.cpp
    ./Plugins/Pdf/PDFWriter/PDFObjectParser.cpp
    ./Plugins/Pdf/PDFWriter/JPEGImageParser.cpp
    ./Plugins/Pdf/PDFWriter/DecryptionHelper.cpp
    ./Plugins/Pdf/PDFWriter/AbstractWrittenFont.cpp
    ./Plugins/Pdf/PDFWriter/PNGImageHandler.cpp
    ./Plugins/Pdf/PDFWriter/PDFModifiedPage.cpp
    ./Plugins/Pdf/PDFWriter/EncryptionHelper.cpp
    ./Plugins/Pdf/PDFWriter/CharStringType1Interpreter.cpp
    ./Plugins/Pdf/PDFWriter/InputPFBDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/CharStringType2Flattener.cpp
    ./Plugins/Pdf/PDFWriter/ANSIFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/MD5Generator.cpp
    ./Plugins/Pdf/PDFWriter/UnicodeString.cpp
    ./Plugins/Pdf/PDFWriter/IndirectObjectsReferenceRegistry.cpp
    ./Plugins/Pdf/PDFWriter/WrittenFontCFF.cpp
    ./Plugins/Pdf/PDFWriter/ResourcesDictionary.cpp
    ./Plugins/Pdf/PDFWriter/PDFUsedFont.cpp
    ./Plugins/Pdf/PDFWriter/CharStringType2Tracer.cpp
    ./Plugins/Pdf/PDFWriter/XCryptionCommon.cpp
    ./Plugins/Pdf/PDFWriter/CFFPrimitiveWriter.cpp
    ./Plugins/Pdf/PDFWriter/PDFParserTokenizer.cpp
    ./Plugins/Pdf/PDFWriter/WrittenFontTrueType.cpp
    ./Plugins/Pdf/PDFWriter/UsedFontsRepository.cpp
    ./Plugins/Pdf/PDFWriter/CIDFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/DescendentFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/InputDCTDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/CFFPrimitiveReader.cpp
    ./Plugins/Pdf/PDFWriter/PDFDocEncoding.cpp
    ./Plugins/Pdf/PDFWriter/PrimitiveObjectsWriter.cpp
    ./Plugins/Pdf/PDFWriter/PDFDate.cpp
    ./Plugins/Pdf/PDFWriter/FreeTypeOpenTypeWrapper.cpp
    ./Plugins/Pdf/PDFWriter/FreeTypeWrapper.cpp
    ./Plugins/Pdf/PDFWriter/FontDescriptorWriter.cpp
    ./Plugins/Pdf/PDFWriter/CharStringType1Tracer.cpp
    ./Plugins/Pdf/PDFWriter/PDFDocumentCopyingContext.cpp
    ./Plugins/Pdf/PDFWriter/OpenTypePrimitiveReader.cpp
    ./Plugins/Pdf/PDFWriter/PFMFileReader.cpp
    ./Plugins/Pdf/PDFWriter/WinAnsiEncoding.cpp
    ./Plugins/Pdf/PDFWriter/PDFPageInput.cpp
    ./Plugins/Pdf/PDFWriter/InputPredictorTIFFSubStream.cpp
    ./Plugins/Pdf/PDFWriter/InputPredictorPNGOptimumStream.cpp
    ./Plugins/Pdf/PDFWriter/OutputFlateEncodeStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFStream.cpp
    ./Plugins/Pdf/PDFWriter/InputFlateDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/CFFANSIFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/InputBufferedStream.cpp
    ./Plugins/Pdf/PDFWriter/CFFDescendentFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/OutputFlateDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/Log.cpp
    ./Plugins/Pdf/PDFWriter/InputAESDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/FreeTypeType1Wrapper.cpp
    ./Plugins/Pdf/PDFWriter/DictionaryContext.cpp
    ./Plugins/Pdf/PDFWriter/OutputAESEncodeStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFTextString.cpp
    ./Plugins/Pdf/PDFWriter/PageTree.cpp
    ./Plugins/Pdf/PDFWriter/PageContentContext.cpp
    ./Plugins/Pdf/PDFWriter/TrueTypeANSIFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/StandardEncoding.cpp
    ./Plugins/Pdf/PDFWriter/InputAscii85DecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/ParsedPrimitiveHelper.cpp
    ./Plugins/Pdf/PDFWriter/StateWriter.cpp
    ./Plugins/Pdf/PDFWriter/Type2CharStringWriter.cpp
    ./Plugins/Pdf/PDFWriter/TrueTypeDescendentFontWriter.cpp
    ./Plugins/Pdf/PDFWriter/OutputBufferedStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFPage.cpp
    ./Plugins/Pdf/PDFWriter/TrueTypePrimitiveWriter.cpp
    ./Plugins/Pdf/PDFWriter/InputFileStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFDictionaryIterator.cpp
    ./Plugins/Pdf/PDFWriter/TimersRegistry.cpp
    ./Plugins/Pdf/PDFWriter/TiledPatternContentContext.cpp
    ./Plugins/Pdf/PDFWriter/InfoDictionary.cpp
    ./Plugins/Pdf/PDFWriter/XObjectContentContext.cpp
    ./Plugins/Pdf/PDFWriter/ArrayOfInputStreamsStream.cpp
    ./Plugins/Pdf/PDFWriter/InputByteArrayStream.cpp
    ./Plugins/Pdf/PDFWriter/InputFile.cpp
    ./Plugins/Pdf/PDFWriter/InputCharStringDecodeStream.cpp
    ./Plugins/Pdf/PDFWriter/Trace.cpp
    ./Plugins/Pdf/PDFWriter/InputStringStream.cpp
    ./Plugins/Pdf/PDFWriter/OutputFile.cpp
    ./Plugins/Pdf/PDFWriter/PDFPageMergingHelper.cpp
    ./Plugins/Pdf/PDFWriter/InputStreamSkipperStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFObject.cpp
    ./Plugins/Pdf/PDFWriter/CatalogInformation.cpp
    ./Plugins/Pdf/PDFWriter/OutputStringBufferStream.cpp
    ./Plugins/Pdf/PDFWriter/TrailerInformation.cpp
    ./Plugins/Pdf/PDFWriter/OutputStreamTraits.cpp
    ./Plugins/Pdf/PDFWriter/InputStringBufferStream.cpp
    ./Plugins/Pdf/PDFWriter/OutputFileStream.cpp
    ./Plugins/Pdf/PDFWriter/RC4.cpp
    ./Plugins/Pdf/PDFWriter/PDFRectangle.cpp
    ./Plugins/Pdf/PDFWriter/StateReader.cpp
    ./Plugins/Pdf/PDFWriter/CMYKRGBColor.cpp
    ./Plugins/Pdf/PDFWriter/InputLimitedStream.cpp
    ./Plugins/Pdf/PDFWriter/PDFFormXObject.cpp
    ./Plugins/Pdf/PDFWriter/TiffUsageParameters.cpp
    ./Plugins/Pdf/PDFWriter/PDFTiledPattern.cpp
    ./Plugins/Pdf/PDFWriter/PDFDictionary.cpp
    ./Plugins/Pdf/PDFWriter/OutputRC4XcodeStream.cpp
    ./Plugins/Pdf/PDFWriter/InputRC4XcodeStream.cpp
    ./Plugins/Pdf/PDFWriter/UppercaseSequance.cpp
    ./Plugins/Pdf/PDFWriter/GraphicStateStack.cpp
    ./Plugins/Pdf/PDFWriter/PDFImageXObject.cpp
    ./Plugins/Pdf/PDFWriter/PDFArray.cpp
    ./Plugins/Pdf/PDFWriter/Ascii7Encoding.cpp
    ./Plugins/Pdf/PDFWriter/PSBool.cpp
    ./Plugins/Pdf/PDFWriter/PDFStreamInput.cpp
    ./Plugins/Pdf/PDFWriter/GraphicState.cpp
    ./Plugins/Pdf/PDFWriter/PDFCosDict.cpp
    ./Plugins/Pdf/PDFWriter/JPEGImageInformation.cpp
    ./Plugins/Pdf/PDFWriter/Timer.cpp
    ./Plugins/Pdf/PDFWriter/PDFLiteralString.cpp
    ./Plugins/Pdf/PDFWriter/RefCountObject.cpp
    ./Plugins/Pdf/PDFWriter/PDFArrayIterator.cpp
    ./Plugins/Pdf/PDFWriter/PDFHexString.cpp
    ./Plugins/Pdf/PDFWriter/PDFIndirectObjectReference.cpp
    ./Plugins/Pdf/PDFWriter/PDFInteger.cpp
    ./Plugins/Pdf/PDFWriter/PDFName.cpp
    ./Plugins/Pdf/PDFWriter/PDFBoolean.cpp
    ./Plugins/Pdf/PDFWriter/PDFReal.cpp
    ./Plugins/Pdf/PDFWriter/PDFSymbol.cpp
    ./Plugins/Pdf/PDFWriter/EncryptionOptions.cpp
    ./Plugins/Pdf/PDFWriter/PDFParsingOptions.cpp
    ./Plugins/Pdf/PDFWriter/PDFCosArray.cpp
    ./Plugins/Pdf/PDFWriter/PDFNull.cpp
    ./Plugins/Unix/unix_sys_utils.cpp
    ./Plugins/Sqlite3/sqlite3.cpp
    ./Plugins/Updater/tm_updater.cpp
    ./Plugins/Updater/tm_winsparkle.cpp
)

### --------------------------------------------------------------------
### the experimental style extension
### --------------------------------------------------------------------


SET(TeXmacs_Style_SRC
    ./Style/Environment/assoc_environment.cpp
    ./Style/Environment/basic_environment.cpp
    ./Style/Environment/list_environment.cpp
    ./Style/Environment/std_environment.cpp
    ./Style/Evaluate/evaluate_boolean.cpp
    ./Style/Evaluate/evaluate_control.cpp
    ./Style/Evaluate/evaluate_inactive.cpp
    ./Style/Evaluate/evaluate_length.cpp
    ./Style/Evaluate/evaluate_macro.cpp
    ./Style/Evaluate/evaluate_main.cpp
    ./Style/Evaluate/evaluate_misc.cpp
    ./Style/Evaluate/evaluate_numeric.cpp
    ./Style/Evaluate/evaluate_quote.cpp
    ./Style/Evaluate/evaluate_rewrite.cpp
    ./Style/Evaluate/evaluate_textual.cpp
    ./Style/Memorizer/clean_copy.cpp
    ./Style/Memorizer/memorizer.cpp
)


### --------------------------------------------------------------------
### X11 interface
### --------------------------------------------------------------------

SET(TeXmacs_Widkit_SRCS
    ./Plugins/Widkit/Attribute/attribute_widget.cpp
    ./Plugins/Widkit/Basic/basic_widget.cpp
    ./Plugins/Widkit/Basic/layout.cpp
    ./Plugins/Widkit/Basic/simple_wk_widget.cpp
    ./Plugins/Widkit/Basic/widkit_wrapper.cpp
    ./Plugins/Widkit/Basic/window_widget.cpp
    ./Plugins/Widkit/Basic/wk_widget.cpp
    ./Plugins/Widkit/Button/button_widget.cpp
    ./Plugins/Widkit/Button/popup_button.cpp
    ./Plugins/Widkit/Button/popup_widget.cpp
    ./Plugins/Widkit/Button/toggle_widget.cpp
    ./Plugins/Widkit/Composite/aligned_widget.cpp
    ./Plugins/Widkit/Composite/array_widget.cpp
    ./Plugins/Widkit/Composite/composite_widget.cpp
    ./Plugins/Widkit/Composite/extend_widget.cpp
    ./Plugins/Widkit/Composite/list_widget.cpp
    ./Plugins/Widkit/Composite/minibar_widget.cpp
    ./Plugins/Widkit/Composite/split_widget.cpp
    ./Plugins/Widkit/Composite/switch_widget.cpp
    ./Plugins/Widkit/Composite/tabs_widget.cpp
    ./Plugins/Widkit/Composite/tile_widget.cpp
    ./Plugins/Widkit/Event/attribute_event.cpp
    ./Plugins/Widkit/Event/basic_event.cpp
    ./Plugins/Widkit/Event/composite_event.cpp
    ./Plugins/Widkit/Event/scroll_event.cpp
    ./Plugins/Widkit/Input/choice_widget.cpp
    ./Plugins/Widkit/Input/enum_widget.cpp
    ./Plugins/Widkit/Input/input_widget.cpp
    ./Plugins/Widkit/Input/inputs_list_widget.cpp
    ./Plugins/Widkit/Misc/balloon_widget.cpp
    ./Plugins/Widkit/Misc/color_picker_widget.cpp
    ./Plugins/Widkit/Misc/file_chooser_widget.cpp
    ./Plugins/Widkit/Misc/ink_widget.cpp
    ./Plugins/Widkit/Misc/refresh_widget.cpp
    ./Plugins/Widkit/Misc/texmacs_widget.cpp
    ./Plugins/Widkit/Misc/wait_widget.cpp
    ./Plugins/Widkit/Output/glue_widget.cpp
    ./Plugins/Widkit/Output/separator_widget.cpp
    ./Plugins/Widkit/Output/text_widget.cpp
    ./Plugins/Widkit/Output/xpm_widget.cpp
    ./Plugins/Widkit/Scrollable/canvas_widget.cpp
    ./Plugins/Widkit/Scrollable/scroll_widget.cpp
    ./Plugins/Widkit/Scrollable/scrollable_widget.cpp
    ./Plugins/Widkit/Scrollable/scrollbar_widget.cpp
)

SET(TeXmacs_X11_SRCS
    ./Plugins/X11/x_drawable.cpp
    ./Plugins/X11/x_font.cpp
    ./Plugins/X11/x_gui.cpp
    ./Plugins/X11/x_init.cpp
    ./Plugins/X11/x_loop.cpp
    ./Plugins/X11/x_shadow.cpp
    ./Plugins/X11/x_window.cpp
    ./Plugins/X11/x_picture.cpp
    ./Plugins/Ghostscript/ghostscript.cpp
)

### --------------------------------------------------------------------
### Qt interface
### --------------------------------------------------------------------

SET(TeXmacs_Qt_SRCS
    ./Plugins/Qt/qt_chooser_widget.cpp
    ./Plugins/Qt/qt_color_picker_widget.cpp
    ./Plugins/Qt/qt_dialogues.cpp
    ./Plugins/Qt/qt_font.cpp
    ./Plugins/Qt/qt_gui.cpp
    ./Plugins/Qt/qt_menu.cpp
    ./Plugins/Qt/qt_picture.cpp
    ./Plugins/Qt/qt_pipe_link.cpp
    ./Plugins/Qt/qt_printer_widget.cpp
    ./Plugins/Qt/qt_renderer.cpp
    ./Plugins/Qt/qt_simple_widget.cpp
    ./Plugins/Qt/qt_sys_utils.cpp
    ./Plugins/Qt/qt_tm_widget.cpp
    ./Plugins/Qt/qt_ui_element.cpp
    ./Plugins/Qt/qt_utilities.cpp
    ./Plugins/Qt/qt_widget.cpp
    ./Plugins/Qt/qt_window_widget.cpp
    ./Plugins/Qt/QTMFileDialog.cpp
    ./Plugins/Qt/QTMGuiHelper.cpp
    ./Plugins/Qt/QTMInteractivePrompt.cpp
    ./Plugins/Qt/QTMMenuHelper.cpp
    ./Plugins/Qt/QTMPipeLink.cpp
    ./Plugins/Qt/QTMPrintDialog.cpp
    ./Plugins/Qt/QTMPrinterSettings.cpp
    ./Plugins/Qt/QTMScrollView.cpp
    ./Plugins/Qt/QTMSockets.cpp
    ./Plugins/Qt/QTMStyle.cpp
    ./Plugins/Qt/QTMTreeModel.cpp
    ./Plugins/Qt/QTMWidget.cpp
    ./Plugins/Qt/QTMWindow.cpp
)

SET(TeXmacs_Qt_Moc_HDRS
    ./Plugins/Qt/QTMApplication.hpp
    ./Plugins/Qt/QTMFileDialog.hpp
    ./Plugins/Qt/QTMGuiHelper.hpp
    ./Plugins/Qt/QTMInteractiveInputHelper.hpp
    ./Plugins/Qt/QTMInteractivePrompt.hpp
    ./Plugins/Qt/QTMMenuHelper.hpp
    ./Plugins/Qt/QTMPipeLink.hpp
    ./Plugins/Qt/QTMPrintDialog.hpp
    ./Plugins/Qt/QTMPrinterSettings.hpp
    ./Plugins/Qt/QTMScrollView.hpp
    ./Plugins/Qt/QTMSockets.hpp
    ./Plugins/Qt/QTMStyle.hpp
    ./Plugins/Qt/QTMTreeModel.hpp
    ./Plugins/Qt/QTMWidget.hpp
    ./Plugins/Qt/QTMWindow.hpp
)


### --------------------------------------------------------------------
### Aqua interface
### --------------------------------------------------------------------

SET(TeXmacs_Cocoa_SRCS
    ./Plugins/Cocoa/aqua_dialogues.mm
    ./Plugins/Cocoa/aqua_gui.mm
    ./Plugins/Cocoa/aqua_menu.mm
    ./Plugins/Cocoa/aqua_renderer.mm
    ./Plugins/Cocoa/aqua_utilities.mm
    ./Plugins/Cocoa/aqua_widget.mm
    ./Plugins/Cocoa/mac_utilities.mm
    ./Plugins/Cocoa/TMButtonsController.m
    ./Plugins/Cocoa/TMView.mm
)

set (TeXmacs_MacOS_SRCS
    ./Plugins/MacOS/HIDRemote.m
    ./Plugins/MacOS/mac_images.mm
    ./Plugins/MacOS/mac_spellservice.mm
    ./Plugins/MacOS/mac_utilities.mm
)

if (APPLE AND TEXMACS_GUI STREQUAL "X11")
    set (TeXmacs_MacOS_SRCS ${TeXmacs_MacOS_SRCS} ./Plugins/MacOS/mac_app.mm)
endif (APPLE AND TEXMACS_GUI STREQUAL "X11")

if (APPLE AND USE_SPARKLE)
    set (TeXmacs_MacOS_SRCS ${TeXmacs_MacOS_SRCS} ./Plugins/Updater/tm_sparkle.mm)
endif (APPLE AND USE_SPARKLE)

# Windows support
# ./Plugins/Windows/mingw_sys_utils.cpp
# ./Plugins/Windows/spawn.cpp



### --------------------------------------------------------------------
### Set variables related to the executable target
### --------------------------------------------------------------------



## setup base source list, library dependencies, include dirs

set (TeXmacs_All_SRCS ${TeXmacs_Base_SRCS} ${TeXmacs_Std_Plugins_SRCS})

set (TeXmacs_Libraries
    ${Guile_LIBRARIES}
    ${ZLIB_LIBRARIES}
    ${JPEG_LIBRARIES}
    ${PNG_LIBRARIES}
    -lpthread)

if (LINKED_CAIRO)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${CAIRO_LIBRARIES})
endif (LINKED_CAIRO)

if (LINKED_SQLITE3)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${SQLITE3_LIBRARIES})
endif (LINKED_SQLITE3)

if (LINKED_FREETYPE)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${FREETYPE_LIBRARIES})
endif (LINKED_FREETYPE)

set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs}
  ${Guile_INCLUDE_DIRS} ${FREETYPE_INCLUDE_DIRS} ${Cairo_INCLUDE_DIRS}
  ${IMLIB2_INCLUDE_DIR}
)


### --------------------------------------------------------------------
### perform gui related operations
### --------------------------------------------------------------------


IF(TEXMACS_GUI STREQUAL "Qt")

  ADD_DEFINITIONS(${QT_DEFINITIONS})
  IF(CMAKE_BUILD_TYPE STREQUAL "Release")
    ADD_DEFINITIONS(-DQT_NO_DEBUG)
  ENDIF(CMAKE_BUILD_TYPE STREQUAL "Release")

  # After this call, TeXmacs_Qt_Moc_SRCS = moc_QTMWidget.cpp ...
  QT5_WRAP_CPP(TeXmacs_Qt_Moc_SRCS ${TeXmacs_Qt_Moc_HDRS})

  SET(TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Qt_SRCS} ${TeXmacs_Qt_Moc_SRCS})
  SET(TeXmacs_Libraries ${TeXmacs_Libraries}  ${QT_LIBRARIES})
  SET(TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${QT_INCLUDES})

ELSEIF(TEXMACS_GUI STREQUAL "Aqua")

  FIND_LIBRARY(COCOA_LIBRARY Cocoa)
  FIND_PATH(COCOA_INCLUDE_DIR Cocoa/Cocoa.h)
  SET(TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Cocoa_SRCS})
  SET(TeXmacs_Libraries ${TeXmacs_Libraries} ${COCOA_LIBRARY})
  SET(TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${COCOA_INCLUDE_DIR})
  
  MARK_AS_ADVANCED(COCOA_LIBRARY)
  MARK_AS_ADVANCED(COCOA_INCLUDE_DIR)

ELSE(TEXMACS_GUI STREQUAL "Qt")

  SET(TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Widkit_SRCS} ${TeXmacs_X11_SRCS})
  SET(TeXmacs_Libraries ${TeXmacs_Libraries} ${X11_LIBRARIES})

ENDIF(TEXMACS_GUI STREQUAL "Qt")

if (APPLE)
    set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_MacOS_SRCS})
    set (TeXmacs_Libraries ${TeXmacs_Libraries} "-framework Carbon -framework Foundation -framework IOKit -framework Cocoa -framework AppKit -framework CoreData")
endif (APPLE)


### --------------------------------------------------------------------
### initialize executable target with all the infos collected so far
### --------------------------------------------------------------------

INCLUDE_DIRECTORIES(${TeXmacs_Include_Dirs}) 

add_executable ( ${TeXmacs_binary_name} ${GUI_TYPE}
  ${TeXmacs_All_SRCS}
)

target_link_libraries (${TeXmacs_binary_name} ${TeXmacs_Libraries})

#--------------------------------------------------------------------------------
#-- Set the Debug and Release names for the libraries
SET_TARGET_PROPERTIES(${TeXmacs_binary_name} 
    PROPERTIES
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
)

install(TARGETS ${TeXmacs_binary_name} DESTINATION lib/texmacs/TeXmacs/bin)
