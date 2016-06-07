
#-------------------------------------------------------------------
# Support for Qt
#-------------------------------------------------------------------

m4_include([misc/autotroll/autotroll.m4])
m4_include([misc/m4/qt5.m4])

AC_DEFUN([HACKED_AT_WITH_QT],[
#QT has the install dir hard coded in library so we need to fix it manually for relocatable environment
if test -z $TMBUILDENV
then
     QT5_AVAILABLE="no"
     SAVE_LIBS="$LIBS"
     LIBS=""
#     AX_HAVE_QT
     LIBS="$SAVE_LIBS"
     if test x"$have_qt" = xyes; then 
        AC_MSG_RESULT([Qt5 found])
	at_cv_qt_build=ok
        QMAKE="qmake"
        MOC="moc"
        QT5_AVAILABLE="yes"
     else
#        AC_MSG_RESULT([Qt5 seems not present, trying Qt4...])
     	AT_WITH_QT
     fi
     # clean and dispatch the collected flags
     LC_COPY_FLAGS([QT],[TMP])
     LC_CLEAR_FLAGS([QT])
     LC_SCATTER_FLAGS([$TMP_CPPFLAGS $TMP_CXXFLAGS $TMP_LDFLAGS $TMP_LIBS $QT_DEFINES],[QT])
     QT_FRAMEWORKS_PATH=`$QMAKE -query QT_INSTALL_LIBS`
     QT_PLUGINS_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
     LC_GET_ARG_VALUE([QT_CXXFLAGS], [-mmacosx-version-min], [MACOSX_VERSION_MIN])
     if [[[ -n $MACOSX_VERSION_MIN ]]]
     then m4_foreach(_tmp1,[[QT_CXXFLAGS], [QT_CPPFLAGS], [QT_LDFLAGS]], [STRIP_ARG(_tmp1,[-mmacosx-version-min])])
     fi
else
    # windows part
    QT_FRAMEWORKS_PATH=/Qt
    QT_PATH=$QT_FRAMEWORKS_PATH/bin
    QT_PLUGINS_PATH=$QT_FRAMEWORKS_PATH/plugins
    QT_LIBS="-L/Qt/lib -lmingw32 -lQtGui4 -lQtCore4 -lpoppler-qt4"
    MOCFLAGS="-DUNICODE -DQT_LARGEFILE_SUPPORT -DQT_DLL -DQT_NO_DEBUG -DQT_GUI_LIB -DQT_CORE_LIB -DQT_HAVE_MMX -DQT_HAVE_3DNOW -DQT_HAVE_SSE -DQT_HAVE_MMXEXT -DQT_HAVE_SSE2 -DQT_THREAD_SUPPORT"
    QT_DEFINES="-DUNICODE -DQT_LARGEFILE_SUPPORT -DQT_DLL -DQT_NO_DEBUG -DQT_GUI_LIB -DQT_CORE_LIB -DQT_HAVE_MMX -DQT_HAVE_3DNOW -DQT_HAVE_SSE -DQT_HAVE_MMXEXT -DQT_HAVE_SSE2 -DQT_THREAD_SUPPORT"
    QT_CPPFLAGS="$QT_DEFINES -I/Qt/include -I/Qt/include/Qt -I/Qt/include/QtCore -I/Qt/include/QtGui"
    QT_CXXFLAGS="-O2 -frtti -fexceptions -mthreads -Wall $QT_DEFINES"
    QT_CFLAGS="-O2 -Wall $QT_DEFINES"
    MOCFLAGS=$QT_DEFINES
    MOC=$QT_PATH/moc
    QMAKE=$QT_PATH/qmake
fi
])
