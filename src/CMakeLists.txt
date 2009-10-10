

# set some variables containing source file lists and project include dirs (excluding external libraries)

### --------------------------------------------------------------------
### the include dirs
### --------------------------------------------------------------------

SET(TeXmacs_Include_Dirs
    ./System
    ./System/Boot
    ./System/Classes
    ./System/Files
    ./System/Link
    ./System/Misc
    ./System/Language
    ./Kernel/Abstractions
    ./Kernel/Containers
    ./Kernel/Types
    ./Data/Convert
    ./Data/Drd
    ./Data/Observers
    ./Data/String
    ./Data/Tmfs
    ./Data/Tree
    ./Guile
    ./Graphics/Bitmap_fonts
    ./Graphics/Fonts
    ./Graphics/Gui
    ./Graphics/Mathematics
    ./Graphics/Renderer
    ./Plugins
    ./Typeset
    ./Typeset/Bridge
    ./Typeset/Concat
    ./Typeset/Page
    ./Texmacs
    ./Edit
    ./Edit/Editor
    ./Edit/Interface
    ./Edit/Modify
    ${TEXMACS_SOURCE_DIR}/TeXmacs/include 
    ${TEXMACS_BINARY_DIR}/src/System/ 
)


### --------------------------------------------------------------------
### the main sources
### --------------------------------------------------------------------


SET(TeXmacs_Base_SRCS 
    ./Data/Convert/Generic/generic.cpp
    ./Data/Convert/Generic/input.cpp
    ./Data/Convert/Scheme/from_scheme.cpp
    ./Data/Convert/Scheme/to_scheme.cpp
    ./Data/Convert/Tex/fromtex.cpp
    ./Data/Convert/Tex/inittex.cpp
    ./Data/Convert/Tex/parsetex.cpp
    ./Data/Convert/Texmacs/fromtm.cpp
    ./Data/Convert/Texmacs/totm.cpp
    ./Data/Convert/Texmacs/upgradetm.cpp
    ./Data/Convert/Verbatim/verbatim.cpp
    ./Data/Convert/Xml/fromxml.cpp
    ./Data/Convert/Xml/parsexml.cpp
    ./Data/Drd/drd_info.cpp
    ./Data/Drd/drd_mode.cpp
    ./Data/Drd/drd_std.cpp
    ./Data/Drd/tag_info.cpp
    ./Data/Drd/vars.cpp
    ./Data/Observers/ip_observer.cpp
    ./Data/Observers/link.cpp
    ./Data/Observers/list_observer.cpp
    ./Data/Observers/tree_pointer.cpp
    ./Data/Observers/tree_position.cpp
    ./Data/String/analyze.cpp
    ./Data/String/converter.cpp
    ./Data/Tmfs/collection.cpp
    ./Data/Tmfs/disk_table.cpp
    ./Data/Tmfs/properties.cpp
    ./Data/Tmfs/solutions.cpp
    ./Data/Tmfs/tmfs_convert.cpp
    ./Data/Tmfs/tmfs_file.cpp
    ./Data/Tmfs/tmfs_permissions.cpp
    ./Data/Tmfs/tmfs_properties.cpp
    ./Data/Tmfs/tmfs_table.cpp
    ./Data/Tmfs/tmfs_versioning.cpp
    ./Data/Tmfs/transaction.cpp
    ./Data/Tree/tree_correct.cpp
    ./Data/Tree/tree_cursor.cpp
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
    ./Graphics/Bitmap_fonts/glyph_ops.cpp
    ./Graphics/Bitmap_fonts/glyph_shrink.cpp
    ./Graphics/Fonts/charmap.cpp
    ./Graphics/Fonts/compound_font.cpp
    ./Graphics/Fonts/find_font.cpp
    ./Graphics/Fonts/font.cpp
    ./Graphics/Fonts/math_font.cpp
    ./Graphics/Fonts/translator.cpp
    ./Graphics/Fonts/virtual_font.cpp
    ./Graphics/Gui/widget.cpp
    ./Graphics/Mathematics/math_tree.cpp
    ./Graphics/Mathematics/test_math.cpp
    ./Graphics/Renderer/basic_renderer.cpp
    ./Graphics/Renderer/page_type.cpp
    ./Graphics/Renderer/printer.cpp
    ./Graphics/Renderer/renderer.cpp
#    ./Guile/Glue/glue.cpp
    ./Guile/Glue/glue_all.cpp
#    ./Guile/Glue/glue_basic.cpp
#    ./Guile/Glue/glue_editor.cpp
#    ./Guile/Glue/glue_server.cpp
    ./Guile/Scheme/evaluate.cpp
    ./Guile/Scheme/object.cpp
    ./Kernel/Abstractions/basic.cpp
    ./Kernel/Abstractions/command.cpp
    ./Kernel/Abstractions/observer.cpp
    ./Kernel/Containers/array.cpp
    ./Kernel/Containers/hashfunc.cpp
    ./Kernel/Containers/hashmap.cpp
    ./Kernel/Containers/hashmap_extra.cpp
    ./Kernel/Containers/hashset.cpp
    ./Kernel/Containers/hashtree.cpp
    ./Kernel/Containers/iterator.cpp
    ./Kernel/Containers/list.cpp
    ./Kernel/Containers/rel_hashmap.cpp
    ./Kernel/Types/parse_string.cpp
    ./Kernel/Types/path.cpp
    ./Kernel/Types/rectangles.cpp
    ./Kernel/Types/space.cpp
    ./Kernel/Types/string.cpp
    ./Kernel/Types/tab.cpp
    ./Kernel/Types/tree.cpp
    ./Kernel/Types/tree_label.cpp
    ./System/Boot/init_texmacs.cpp
    ./System/Boot/init_upgrade.cpp
    ./System/Classes/timer.cpp
    ./System/Classes/url.cpp
    ./System/Files/file.cpp
    ./System/Files/image_files.cpp
    ./System/Files/web_files.cpp
    ./System/Language/cpp_language.cpp
    ./System/Language/dictionary.cpp
    ./System/Language/hyphenate.cpp
    ./System/Language/language.cpp
    ./System/Language/math_language.cpp
    ./System/Language/mathemagix_language.cpp
    ./System/Language/scheme_language.cpp
    ./System/Language/text_language.cpp
    ./System/Language/verb_language.cpp
    ./System/Link/connection.cpp
    ./System/Link/dyn_link.cpp
    ./System/Link/pipe_link.cpp
    ./System/Link/socket_link.cpp
    ./System/Link/socket_server.cpp
    ./System/Link/texmacs_client.cpp
    ./System/Link/texmacs_server.cpp
    ./System/Link/tm_link.cpp
    ./System/Misc/data_cache.cpp
    ./System/Misc/fast_alloc.cpp
    ./System/Misc/sys_utils.cpp
    ./Texmacs/Data/tm_data.cpp
    ./Texmacs/Data/tm_file.cpp
    ./Texmacs/Server/tm_config.cpp
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
    ./Typeset/Env/env_default.cpp
    ./Typeset/Env/env_exec.cpp
    ./Typeset/Env/env_inactive.cpp
    ./Typeset/Env/env_length.cpp
    ./Typeset/Env/env_semantics.cpp
    ./Typeset/Format/format.cpp
    ./Typeset/Format/formatter.cpp
    ./Typeset/Format/line_item.cpp
    ./Typeset/Format/page_item.cpp
    ./Typeset/Graphics/curve.cpp
    ./Typeset/Graphics/equations.cpp
    ./Typeset/Graphics/frame.cpp
    ./Typeset/Graphics/grid.cpp
    ./Typeset/Graphics/point.cpp
    ./Typeset/Line/lazy_gui.cpp
    ./Typeset/Line/lazy_paragraph.cpp
    ./Typeset/Line/lazy_typeset.cpp
    ./Typeset/Line/lazy_vstream.cpp
    ./Typeset/Line/line_breaker.cpp
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
    ./Plugins/Cairo/cairo_renderer.cpp
    ./Plugins/Cairo/tm_cairo.cpp
    ./Plugins/Freetype/free_type.cpp
    ./Plugins/Freetype/tt_face.cpp
    ./Plugins/Freetype/tt_file.cpp
    ./Plugins/Freetype/tt_font.cpp
    ./Plugins/Freetype/unicode_font.cpp
    ./Plugins/Imlib2/imlib2.cpp
    ./Plugins/Ispell/ispell.cpp
    ./Plugins/Metafont/load_pk.cpp
    ./Plugins/Metafont/load_tex.cpp
    ./Plugins/Metafont/load_tfm.cpp
    ./Plugins/Metafont/tex_files.cpp
    ./Plugins/Metafont/tex_font.cpp
    ./Plugins/Metafont/tex_init.cpp
    ./Plugins/Metafont/tex_rubber_font.cpp
    ./Plugins/Mplayer/mplayer.cpp
    ./Plugins/Openssl/openssl.cpp
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
    ./Plugins/Widkit/Composite/array_widget.cpp
    ./Plugins/Widkit/Composite/composite_widget.cpp
    ./Plugins/Widkit/Composite/list_widget.cpp
    ./Plugins/Widkit/Composite/switch_widget.cpp
    ./Plugins/Widkit/Composite/tile_widget.cpp
    ./Plugins/Widkit/Event/attribute_event.cpp
    ./Plugins/Widkit/Event/basic_event.cpp
    ./Plugins/Widkit/Event/composite_event.cpp
    ./Plugins/Widkit/Event/scroll_event.cpp
    ./Plugins/Widkit/Input/input_widget.cpp
    ./Plugins/Widkit/Input/inputs_list_widget.cpp
    ./Plugins/Widkit/Misc/balloon_widget.cpp
    ./Plugins/Widkit/Misc/file_chooser_widget.cpp
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
    ./Plugins/Ghostscript/ghostscript.cpp
)


### --------------------------------------------------------------------
### Qt interface
### --------------------------------------------------------------------

SET(TeXmacs_Qt_SRCS
    ./Plugins/Qt/qt_dialogues.cpp
    ./Plugins/Qt/qt_gui.cpp
    ./Plugins/Qt/qt_menu.cpp
    ./Plugins/Qt/qt_renderer.cpp
    ./Plugins/Qt/qt_utilities.cpp
    ./Plugins/Qt/qt_widget.cpp
    ./Plugins/Qt/QTMMenuHelper.cpp
    ./Plugins/Qt/QTMStyle.cpp
    ./Plugins/Qt/QTMWidget.cpp
)

SET(TeXmacs_Qt_Moc_HDRS
    ./Plugins/Qt/QTMGuiHelper.hpp
    ./Plugins/Qt/QTMInteractiveInputHelper.hpp
    ./Plugins/Qt/QTMMenuHelper.hpp
    ./Plugins/Qt/QTMStyle.hpp
    ./Plugins/Qt/QTMWidget.hpp
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

### --------------------------------------------------------------------
### Set variables related to the executable target
### --------------------------------------------------------------------



## setup base source list, library dependencies, include dirs

SET(TeXmacs_All_SRCS ${TeXmacs_Base_SRCS} ${TeXmacs_Std_Plugins_SRCS})

SET(TeXmacs_Libraries ${Guile_LIBRARIES} ${FREETYPE_LIBRARIES})

SET(TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} 
  ${Guile_INCLUDE_DIRS} ${Freetype_INCLUDE_DIRS} ${Cairo_INCLUDE_DIRS}
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
  QT4_WRAP_CPP(TeXmacs_Qt_Moc_SRCS ${TeXmacs_Qt_Moc_HDRS})

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



### --------------------------------------------------------------------
### initialize executable target with all the infos collected so far
### --------------------------------------------------------------------


INCLUDE_DIRECTORIES(${TeXmacs_Include_Dirs}) 

ADD_EXECUTABLE ( ${TeXmacs_binary_name} ${GUI_TYPE} 
  ${TeXmacs_All_SRCS}
)

TARGET_LINK_LIBRARIES(${TeXmacs_binary_name} ${TeXmacs_Libraries})

#--------------------------------------------------------------------------------
#-- Set the Debug and Release names for the libraries
SET_TARGET_PROPERTIES(${TeXmacs_binary_name} 
    PROPERTIES
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
)



