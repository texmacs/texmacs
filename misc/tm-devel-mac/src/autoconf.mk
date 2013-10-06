# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# gettext
PKG             := autoconf
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 2.69
$(PKG)_CHECKSUM := 562471cbcb0dd0fa42a76665acf0dbb68479b78a
$(PKG)_SUBDIR   := autoconf-$($(PKG)_VERSION)
$(PKG)_FILE     := autoconf-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.gnu.org/software/autoconf/
$(PKG)_URL      := http://ftp.gnu.org/gnu/autoconf/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc libiconv

define $(PKG)_UPDATE
    curl -s -L 'http://www.gnu.org/software/autoconf/' | \
    grep 'gettext-' | \
    $(SED) -n 's,.*gettext-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
    cd '$(1)' && ./configure \
        --prefix='$(PREFIX)' 
    $(MAKE) -C '$(1)' -j '$(JOBS)' install
    $(MAKE) -C '$(1)' -j 1 install
endef


