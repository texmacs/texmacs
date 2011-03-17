# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# GNU Guile
PKG             := guile
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 1.8.8
$(PKG)_CHECKSUM := 548d6927aeda332b117f8fc5e4e82c39a05704f9
$(PKG)_SUBDIR   := $(PKG)-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.gnu.org/software/$(PKG)/
$(PKG)_URL      := http://ftp.gnu.org/gnu/$(PKG)/$($(PKG)_FILE)
$(PKG)_DEPS     := gmp gettext
#gcc libtool gmp libiconv gettext libunistring gc libffi readline

# these options prevents some misbehaviour in configure due to cross-compilation to ppc from i386
MORE_OPT_ppc := ac_cv_type_getgroups=gid_t ac_cv_type_setgroups=gid_t

define $(PKG)_UPDATE
    wget -q -O- 'http://git.savannah.gnu.org/gitweb/?p=$(PKG).git;a=tags' | \
    grep '<a class="list subject"' | \
    $(SED) -n 's,.*<a[^>]*>[^0-9>]*\([0-9][^< ]*\)\.<.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(foreach BUILD_ARCH,$(BUILD_ARCHS),
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH)))
endef

define $(PKG)_BUILD_ARCH
    [ -d '$(1)/../$(3)' ] || mkdir -p '$(1)/../$(3)'
    # The setting "scm_cv_struct_timespec=no" ensures that Guile
    # won't try to use the "struct timespec" from <pthreads.h>,
    # which would fail because we tell Guile not to use Pthreads.
    cd '$(1)/../$(3)' && \
        PKG_CONFIG_PATH=$(PREFIX)/$(3)/lib/pkgconfig/  '$(1)/configure' \
        --host='$(TARGET_$(3))' \
        --prefix='$(PREFIX)/$(3)' \
        --disable-shared \
        --without-threads \
        scm_cv_struct_timespec=no \
        CC="gcc-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CXX="g++-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CPP="cpp-4.2"\
        CXXCPP="cpp-4.2" \
        CPPFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CXXFLAGS="-I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        LDFLAGS="-L$(PREFIX)/$(3)/lib -L$(PREFIX)/lib" \
        $(MORE_OPT_$(3))

    $(MAKE) -C '$(1)/../$(3)' -j '$(JOBS)' schemelib_DATA=
    $(MAKE) -C '$(1)/../$(3)' -j 1 install schemelib_DATA=

#         LIBS='-lunistring -lintl -liconv' \

#    '$(TARGET)-gcc' \
#        -W -Wall -Werror -ansi -pedantic \
#        '$(2).c' -o '$(PREFIX)/$(TARGET)/bin/test-guile.exe' \
#        `'$(TARGET)-pkg-config' guile-1.8 --cflags --libs`
endef
