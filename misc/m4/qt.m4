#-------------------------------------------------------------------
# Support for Qt
#-------------------------------------------------------------------

m4_include([misc/autotroll/autotroll.m4])

AC_DEFUN([HACKED_AT_WITH_QT],[
#QT has the install dir hard coded in library so we need to fix it manually for relocatable environment
if test -z $TMBUILDENV
then AT_WITH_QT
     # MacOS specific: (FIXME! shouldn't we be using qmake -query everywhere?)
     QT_FRAMEWORKS_PATH=`$QMAKE -query QT_INSTALL_LIBS`
     QT_PLUGINS_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
     if [[[ $QT_CFLAGS =~ mmacosx-version-min= ]]]
     then MACOSX_DEPLOYMENT_TARGET="${QT_CFLAGS#*mmacosx-version-min=}" 
        MACOSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET%% *}
     fi
     AC_SUBST(MACOSX_DEPLOYMENT_TARGET)
else
    #windows part
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
