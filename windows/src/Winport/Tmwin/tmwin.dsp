# Microsoft Developer Studio Project File - Name="tmwin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=tmwin - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "tmwin.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "tmwin.mak" CFG="tmwin - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "tmwin - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "tmwin - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "tmwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "tmwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../System/Boot" /I "../../TeXmacs" /I "../../Edit" /I "../../System/Link" /I "../../Guile" /I "../../Classes/Atomic" /I "../../Classes/Abstract" /I "../Sysdep" /I "../../Window" /I "../../Classes/Compound" /I "../../System/Classes" /I "../../Resource" /I "../../System/Misc" /I "../../Typeset" /I "../../Data/Tree" /I "../../Data/String" /I "../../Data/Drd" /I "../../Plugins" /I "../../System/Files" /I "../../Data/Convert" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "OS_WIN32" /FR /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 Ws2_32.lib libguile.lib CxImage.lib j2k.lib jasper.lib jpeg.lib png.lib gdi32.lib user32.lib kernel32.lib shell32.lib freeimage.lib tiff.lib comctl32.lib zlib.lib freetype214.lib /nologo /stack:0x800000 /subsystem:windows /debug /machine:I386 /nodefaultlib:"nafxcwd.lib" /out:"../Bin/tmwin.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "tmwin - Win32 Release"
# Name "tmwin - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\Data\String\analyze.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\array.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Composite\array_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\attribute_event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Attribute\attribute_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Misc\balloon_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\basic.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Basic\basic_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\basic_event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Basic\basic_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Bibtex\bibtex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Bitmap_fonts\bitmap_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Basic\boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_argument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_auto.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_compound.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_default.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_docrange.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_document.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_eval.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_formatting.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_mark.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_rewrite.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_surround.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge_with.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Button\button_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Scrollable\canvas_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Table\cell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Modifier\change_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\command.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\composite_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\composite_event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Composite\composite_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Fonts\compound_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_active.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\concat_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_graphics.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_inactive.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_macro.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_math.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_post.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concat_text.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concater.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Connections\connection.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Converters\converter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\curve.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\decoration_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Dictionaries\dictionary.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\dirent.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\dlfcn.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\drd_info.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\drd_std.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\dyn_link.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Process\edit_aux.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_complete.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_cursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_delete.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_dynamic.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_footer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_graphics.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_interface.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_keyboard.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Editor\edit_main.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_math.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_modify.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_mouse.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Replace\edit_search.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Replace\edit_select.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Process\edit_session.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Replace\edit_spell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_table.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_text.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Editor\edit_typeset.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Encodings\encoding.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env_default.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env_exec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env_inactive.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env_length.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Env\env_semantics.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\equations.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Scheme\evaluate.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Misc\fast_alloc.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\file.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Misc\file_chooser_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Fonts\find_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Fonts\font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\format.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\formatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\frame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\free_type.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Scheme\from_scheme.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Tex\fromtex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Texmacs\fromtm.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Xml\fromxml.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Generic\generic.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Ghostscript\ghostscript.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue_all.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue_basic.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue_editor.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue_server.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Output\glue_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Bitmap_fonts\glyph.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Bitmap_fonts\glyph_ops.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Bitmap_fonts\glyph_shrink.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Graphics\graphics_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\grid.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Graphics\grid_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashfunc.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashmap.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashmap_extra.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashset.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashtree.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\hyphenate.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\image_files.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Boot\init_texmacs.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Boot\init_upgrade.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Tex\inittex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Generic\input.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Input\input_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\ip_observer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Ispell\ispell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\iterator.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\language.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Basic\layout.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_paragraph.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_typeset.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_vstream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\line_breaker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\line_item.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\list.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Composite\list_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_pk.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_tex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_tfm.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\make_pages.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\math_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Fonts\math_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\math_language.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\misc_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Modifier\modifier_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Scheme\object.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\observer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\page_breaker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\page_item.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\PsDevice\page_type.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\pager.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Tex\parsetex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Xml\parsexml.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\path.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\pipe_link.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\point.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\polynomial.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Button\popup_button.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Button\popup_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\PsDevice\printer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\PsDevice\ps_device.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\rectangles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\rel_hashmap.cpp
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Basic\rubber_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\script_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\scroll_event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Scrollable\scroll_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Scrollable\scrollable_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Scrollable\scrollbar_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Output\separator_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\skeleton.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\socket.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\socket_link.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\space.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\stack_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Stack\stacker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Basic\stretch_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\string.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Composite\switch_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Misc\sys_utils.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\sysmisc.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tab.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Table\table.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\tag_info.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\tex_files.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\tex_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\tex_init.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\tex_rubber_font.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\texlog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Texmacs\texmacs.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Basic\text_boxes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\text_language.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Output\text_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Composite\tile_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Classes\timer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Layout\tm_button.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Configure\tm_config.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Data\tm_data.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Data\tm_file.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Layout\tm_menus.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Scheme\tm_scheme.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Server\tm_server.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Texmacs\Layout\tm_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Scheme\to_scheme.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Texmacs\totm.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Translators\translator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tree.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Tree\tree_cursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tree_label.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\tt_face.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\tt_file.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\tt_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\typesetter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Texmacs\upgradetm.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Classes\url.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\vars.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\verb_language.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Verbatim\verbatim.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Fonts\virtual_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\vpenalty.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Misc\wait_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\web_files.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Basic\widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_display.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_drawable.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_font.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_init.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_loop.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_window.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\X11\XColorTable.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\X11\xlib.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Output\xpm_widget.cpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\X11\xutil.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\Include\sys\_stat.h
# End Source File
# Begin Source File

SOURCE=..\Include\sys\_types.h
# End Source File
# Begin Source File

SOURCE=..\..\Data\String\analyze.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\array.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\attribute_event.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\attribute_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\basic.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\basic_event.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\basic_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Bibtex\bibtex.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\bitmap_font.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Boot\boot.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\boxes.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\bridge.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\Button\button_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\command.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\composite.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\composite_event.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\composite_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Concat\concater.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\connect.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\construct.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\convert.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Tex\convert_tex.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\converter.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\curve.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\dictionary.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\dirent.h
# End Source File
# Begin Source File

SOURCE=..\Include\dirent.hpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\dirent.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\display.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\dlfcn.h
# End Source File
# Begin Source File

SOURCE=..\Include\dlfcn.hpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\dlfcn.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\drd_info.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\drd_std.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\dyn_link.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_cursor.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_dynamic.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Interface\edit_interface.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Editor\edit_main.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_math.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_modify.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Process\edit_process.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Replace\edit_replace.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Replace\edit_select.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_table.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Modify\edit_text.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\Editor\edit_typeset.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Edit\editor.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\encoding.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\env.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\equations.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Scheme\evaluate.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\event.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\event_codes.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Misc\fast_alloc.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\file.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\font.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\format.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\formatter.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\frame.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\free_type.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\FreeImage.h
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Ghostscript\ghostscript.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Glue\glue.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\grid.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\guile.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashfunc.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashmap.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashset.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\hashtree.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\hyphenate.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\image_files.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\Languages\impl_language.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Bridge\impl_typesetter.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\netinet\in.h
# End Source File
# Begin Source File

SOURCE=..\..\Data\Convert\Generic\input.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Ispell\ispell.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\Composite\italic_correct.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\iterator.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\X11\keysym.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\keysym.hpp
# End Source File
# Begin Source File

SOURCE=..\Sysdep\X11\keysym.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\language.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\layout.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_paragraph.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_typeset.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Line\lazy_vstream.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\libguile.h
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\line_item.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\list.hpp
# End Source File
# Begin Source File

SOURCE="..\..\Resource\Tex\load-pk.hpp"
# End Source File
# Begin Source File

SOURCE="..\..\Resource\Tex\load-tfm.hpp"
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_pk.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_tex.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\load_tfm.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\make_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\math_util.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\String\merge_sort.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\sys\misc.h
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Boxes\modifier.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\netdb.h
# End Source File
# Begin Source File

SOURCE=..\..\Guile\Scheme\object.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Abstract\observer.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\page_item.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\PsDevice\page_type.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\pager.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\path.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\pipe_link.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\point.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Graphics\polynomial.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\PsDevice\printer.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\ps_device.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\rectangles.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Compound\rel_hashmap.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\resource.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Guile\scheme.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Event\scroll_event.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\Widget\scroll_widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\skeleton.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\sys\socket.h
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\socket_link.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\space.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Format\stack_border.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Stack\stacker.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\string.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\X11\Sunkeysym.h
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tab.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Table\table.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\tag_info.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Metafont\tex_files.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\texlog.h
# End Source File
# Begin Source File

SOURCE=..\Include\TeXmacs.h
# End Source File
# Begin Source File

SOURCE=..\Include\sys\time.h
# End Source File
# Begin Source File

SOURCE=..\..\System\Classes\timer.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\tm_configure.hpp
# End Source File
# Begin Source File

SOURCE=..\..\System\Link\tm_link.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Resource\translator.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tree.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Tree\tree_cursor.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Classes\Atomic\tree_label.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\tt_face.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Plugins\Freetype\tt_file.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\typesetter.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\unistd.h
# End Source File
# Begin Source File

SOURCE=..\..\System\Classes\url.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Data\Drd\vars.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Typeset\Page\vpenalty.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\sys\wait.h
# End Source File
# Begin Source File

SOURCE=..\..\System\Files\web_files.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\widget.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\window.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_display.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_drawable.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_font.hpp
# End Source File
# Begin Source File

SOURCE=..\..\Window\X\x_window.hpp
# End Source File
# Begin Source File

SOURCE=..\Include\X11\Xatom.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\XColorTable.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\XKeyCodes.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\Xlib.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\Xos.h
# End Source File
# Begin Source File

SOURCE=..\Include\X11\Xutil.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
