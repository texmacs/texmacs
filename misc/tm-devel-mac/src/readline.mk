# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# Readline
PKG             := readline
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 6.2
$(PKG)_CHECKSUM := a9761cd9c3da485eb354175fcc2fe35856bc43ac
$(PKG)_SUBDIR   := readline-$($(PKG)_VERSION)
$(PKG)_FILE     := readline-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://tiswww.case.edu/php/chet/readline/rltop.html
$(PKG)_URL      := http://ftp.gnu.org/gnu/readline/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc pdcurses

define $(PKG)_UPDATE
    curl -s -L 'http://tiswww.case.edu/php/chet/readline/rltop.html' | \
    grep 'readline-' | \
    $(SED) -n 's,.*readline-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef

define $(PKG)_BUILD_ARCH
#    $(SED) -i 's,^ *case SIGQUIT:.*,,' '$(1)/signals.c'
#    $(SED) -i 's,^ *case SIGTSTP:.*,,' '$(1)/signals.c'
    cd '$(1)' && ./configure \
        --host='$(TARGET_$(3))' \
        --disable-shared \
        --prefix='$(PREFIX)' \
        --enable-multibyte \
        --without-purify \
        --without-curses
 #       --with-curses \
 #       LIBS='-lpdcurses'
#    $(MAKE) -C '$(1)' -j '$(JOBS)' install SHARED_LIBS=
    $(MAKE) -C '$(1)' -j '$(JOBS)' install SHARED_LIBS=
endef
