AC_DEFUN([LC_SPARKLE],[
  AC_ARG_WITH(sparkle,
  AS_HELP_STRING([--with-sparkle@<:@=ARG@:>@],
  [with (Win)Sparkle autoupdater [ARG=no]]))

  SAVE_OBJCFLAGS="$OBJCFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_sparkle" = "no" -o "$with_sparkle" = "" ; then
      AC_MSG_RESULT([disabling Sparkle usage])
  else
    case "${host}" in
    *mingw*)  
      AC_MSG_CHECKING([whether we can use the WinSparkle library])
      AC_LANG_PUSH([C])
      CFLAGS="-I $with_sparkle"
      LDFLAGS="-L $with_sparkle"
      LIBS="-lwinsparkle"
      AC_TRY_LINK(
  [
  #include <winsparkle.h>
  #include <winsparkle.h>
  ],[
  win_sparkle_init();
  ],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SPARKLE, 1, [Use WinSparkle library])
      SPARKLE_CFLAGS="$CFLAGS"
      SPARKLE_LDFLAGS="$LDFLAGS $LIBS"
      WINSPARKLE_PATH="$with_sparkle"
      WINSPARKLE_DLL="WinSparkle*.dll"
  ],[
      AC_MSG_RESULT(no) ])
      AC_LANG_POP([C])
      ;;
    *apple*darwin*)
      AC_MSG_CHECKING([whether we can use the Sparkle framework])
      AC_LANG_PUSH([Objective C])
      OBJCFLAGS="-F $with_sparkle"
      LDFLAGS="-F $with_sparkle -framework Sparkle"
      AC_TRY_LINK(
  [
  #include <Cocoa/Cocoa.h>
  #include <Sparkle/Sparkle.h>
  ],[
  SUUpdater* updater;
  ],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SPARKLE, 1, [Use Sparkle framework])
      SPARKLE_CFLAGS="$OBJCFLAGS"
      SPARKLE_LDFLAGS="$LDFLAGS"
      SPARKLE_FRAMEWORK_PATH="$with_sparkle"
      CONFIG_SPARKLE="Updater"
  ],[
      AC_MSG_RESULT(no)]
      CONFIG_SPARKLE="")
      AC_LANG_POP([Objective C])
      ;;
    esac
  fi

  OBJFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  AC_SUBST(SPARKLE_CFLAGS)
  AC_SUBST(SPARKLE_LDFLAGS)
  AC_SUBST(SPARKLE_FRAMEWORK_PATH)
  AC_SUBST(CONFIG_SPARKLE)
  AC_SUBST(WINSPARKLE_DLL)
  AC_SUBST(WINSPARKLE_PATH)
  ])
