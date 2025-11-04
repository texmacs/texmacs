FROM ubuntu:noble AS builder

ARG UID=1000
ARG GID=1000

# RUN userdel -r ubuntu
#
# RUN set -eux; \
# 	groupadd -r -g 999 texmacs-server; \
# 	useradd -r -g texmacs-server -u 999 \
# 		--home-dir=/srv/texmacs-server \
# 		--shell=/bin/bash \
# 		texmacs-server; \
# 	install --verbose --directory --owner texmacs-server --group texmacs-server --mode 1777 /srv/texmacs-server

# Install build tools and dependencies (Qt6)
RUN apt-get update && \
		DEBIAN_FRONTEND=noninteractive apt-get install -y \
		build-essential \
		ccache \
		autoconf \
		automake \
		libtool \
		pkg-config \
		cmake \
		ninja-build \
		git \
		rsync \
		vim \
		gettext \
		texinfo \
		libx11-dev \
		libxpm-dev \
		libxext-dev \
		libpng-dev \
		libfreetype6-dev \
		libgmp-dev \
		libnettle8 \
		libhogweed6 \
		libgnutls28-dev \
		libxcb1-dev \
		libxml2-dev \
		libxkbcommon-dev \
		libfontconfig1-dev \
		ghostscript \
		meson \
		zlib1g \
		qt6-base-dev \
		qt6-base-dev-tools \
		qt6-tools-dev \
		qt6-tools-dev-tools \
		qt6-svg-dev \
		libqt6core6 \
		libqt6gui6 \
		libqt6widgets6 \
		libqt6svg6 \
		libqt6printsupport6 \
		libqt6network6 \
		&& \
		apt-get clean && rm -rf /var/lib/apt/lists/*

ARG BUILD_FROM_CWD=false
ARG TEXMACS_URL=https://github.com/texmacs/texmacs.git
ARG TEXMACS_BRANCH=
ARG TEXMACS_TARBALL=
ARG GUILE_TEXMACS_URL=https://github.com/texmacs/guile-texmacs.git
ARG GUILE_TEXMACS_BRANCH=
ARG GUILE_TEXMACS_TARBALL=

ENV BUILD_FROM_CWD=${BUILD_FROM_CWD}
ENV TEXMACS_URL=${TEXMACS_URL}
ENV TEXMACS_BRANCH=${TEXMACS_BRANCH}
ENV TEXMACS_TARBALL=${TEXMACS_TARBALL}
ENV GUILE_TEXMACS_URL=${GUILE_TEXMACS_URL}
ENV GUILE_TEXMACS_BRANCH=${GUILE_TEXMACS_BRANCH}
ENV GUILE_TEXMACS_TARBALL=${GUILE_TEXMACS_TARBALL}

RUN mkdir -p /opt/texmacs

RUN set -eux; \
	chown -R ubuntu:ubuntu /opt/texmacs

USER ubuntu

# using Go filepath patterns, Docker COPY will not fail if it won't locate any valid source
COPY --chown=ubuntu:ubuntu texmacs.tar.g[z] /tmp/texmacs.tar.gz
COPY --chown=ubuntu:ubuntu guile-texmacs.tar.g[z] /tmp/guile-texmacs.tar.gz
COPY --chown=ubuntu:ubuntu \
	--exclude=*.o \
	--exclude=*.so \
	--exclude=*.a \
	src /tmp/texmacs_src

COPY --chown=ubuntu:ubuntu guile-texmacs /tmp/guile_texmacs


ENV QMAKE=/usr/lib/qt6/bin/qmake6

WORKDIR /opt

ARG TEXMACS_ROOT=/opt/texmacs
ARG TEXMACS_SRC=$TEXMACS_ROOT/src
ARG GUILE_SRC_DIR=$TEXMACS_SRC/tm-guile188
ARG TEXMACS_TMP_CLONE=$(mktemp -d)

RUN set -eux; \
    TEXMACS_TMP_CLONE="$(mktemp -d)"; \
	if [ $BUILD_FROM_CWD ]; then \
      mv "/tmp/texmacs_src" "$TEXMACS_SRC"; \
    elif [ -n "$TEXMACS_TARBALL" ]; then \
      if [ -f "$TEXMACS_TARBALL" ]; then \
        echo "Using local texmacs tarball: $TEXMACS_TARBALL"; \
        tar -xzf "$TEXMACS_TARBALL" -C "$TEXMACS_TMP_CLONE"; \
      elif echo "$TEXMACS_TARBALL" | grep -q '^http'; then \
        curl -L "$TEXMACS_TARBALL" -o /tmp/texmacs.tar.gz; \
        tar -xzf /tmp/texmacs.tar.gz -C "$TEXMACS_TMP_CLONE"; \
      else \
        echo "Local tarball path $TEXMACS_TARBALL not found in build context"; exit 1; \
      fi; \
    else \
      if [ -z "$TEXMACS_BRANCH" ]; then \
        git clone "$TEXMACS_URL" "$TEXMACS_TMP_CLONE"; \
      else \
        git clone --branch "$TEXMACS_BRANCH" "$TEXMACS_URL" "$TEXMACS_TMP_CLONE"; \
      fi; \
    fi; \
	if [ $BUILD_FROM_CWD ]; then \
      rm "$GUILE_SRC_DIR"; \
      mv "/tmp/guile_texmacs" "$GUILE_SRC_DIR"; \
    elif [ -d "$TEXMACS_TMP_CLONE/guile-texmacs" ]; then \
      mkdir -p "$TEXMACS_ROOT"; \
      mv "$TEXMACS_TMP_CLONE/src" "$TEXMACS_SRC"; \
      mv "$TEXMACS_TMP_CLONE/guile-texmacs" "$GUILE_SRC_DIR"; \
    else \
      mv "$TEXMACS_TMP_CLONE" "$TEXMACS_SRC"; \
      if [ -n "$GUILE_TEXMACS_TARBALL" ]; then \
        if [ -f "$GUILE_TEXMACS_TARBALL" ]; then \
          echo "Using local guile-texmacs tarball: $GUILE_TEXMACS_TARBALL"; \
          tar -xzf "$GUILE_TEXMACS_TARBALL" -C "$GUILE_SRC_DIR" --strip-components=1; \
        elif echo "$GUILE_TEXMACS_TARBALL" | grep -q '^http'; then \
          curl -L "$GUILE_TEXMACS_TARBALL" -o /tmp/guile-texmacs.tar.gz; \
          tar -xzf /tmp/guile-texmacs.tar.gz -C "$GUILE_SRC_DIR" --strip-components=1; \
        else \
          echo "Local guile-texmacs tarball path $GUILE_TEXMACS_TARBALL not found in build context"; exit 1; \
        fi; \
      else \
        if [ -z "$GUILE_TEXMACS_BRANCH" ]; then \
          git clone "$GUILE_TEXMACS_URL" "$GUILE_SRC_DIR"; \
        else \
          git clone --branch "$GUILE_TEXMACS_BRANCH" "$GUILE_TEXMACS_URL" "$GUILE_SRC_DIR"; \
        fi; \
      fi; \
    fi

WORKDIR /opt/texmacs/src
#RUN --mount=type=cache,target=/home/ubuntu/.cache/ccache/ ccache -s && ccache -p
# RUN --mount=type=cache,target=/home/ubuntu/.cache/ccache/ ls -lad /home/ubuntu/.cache/ccache && exit 1
#RUN --mount=type=cache,target=/home/ubuntu/.cache/ccache/ autoreconf -fi \
RUN autoreconf -fi \
	&& ./configure --with-gnutls=yes --with-guile=embedded18 \
	&& make clean \
	&& make CXX='ccache g++' CC='ccache gcc' -j$(nproc)

FROM ubuntu:noble

# utilities
RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		iproute2 \
		netcat-openbsd \
		gnupg \
		mailutils \
	; \
	rm -rf /var/lib/apt/lists/*

RUN set -eux; \
	groupadd -r -g 999 texmacs-server; \
	useradd -r -g texmacs-server -u 999 \
		--home-dir=/srv/texmacs-server \
		--shell=/bin/bash \
		texmacs-server; \
	install --verbose --directory --owner texmacs-server --group texmacs-server --mode 1777 /srv/texmacs-server

ENV GOSU_VERSION=1.17
RUN set -eux; \
	savedAptMark="$(apt-mark showmanual)"; \
	apt-get update; \
	apt-get install -y --no-install-recommends ca-certificates wget; \
	rm -rf /var/lib/apt/lists/*; \
	dpkgArch="$(dpkg --print-architecture | awk -F- '{ print $NF }')"; \
	wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch"; \
	wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc"; \
	export GNUPGHOME="$(mktemp -d)"; \
	gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4; \
	gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu; \
	gpgconf --kill all; \
	rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc; \
	apt-mark auto '.*' > /dev/null; \
	[ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; \
	apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false; \
	chmod +x /usr/local/bin/gosu; \
	gosu --version; \
	gosu nobody true

# build texmacs with Qt6 and server capabilities before building container
RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		libqt6core6 \
		libqt6gui6 \
		libqt6widgets6 \
		libqt6svg6 \
		libqt6printsupport6 \
		libqt6network6 \
		libltdl7 \
		libfreetype6 \
		libc6 \
		libcrypt1 \
		libgcc-s1 \
		libgmp10 \
		libpng16-16 \
		libstdc++6 \
		zlib1g \
		ghostscript \
		findutils \
		fonts-stix \
		fonts-texgyre \
		libgnutls30 \
	; \
	rm -rf /var/lib/apt/lists/*

# optional tools
RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		vim \
	; \
	rm -rf /var/lib/apt/lists/*

RUN set -eux; \
	mkdir -p \
		/usr/local/share/TeXmacs \
		/usr/local/libexec/TeXmacs/bin \
		/usr/local/libexec/TeXmacs/lib \
		/srv/texmacs-server/.TeXmacs/system \
		/srv/texmacs-server/.TeXmacs/server;

COPY --from=builder /opt/texmacs/src/TeXmacs/progs /usr/local/share/TeXmacs/progs/
COPY --from=builder /opt/texmacs/src/tm-guile188/ice-9 /usr/local/share/TeXmacs/progs/ice-9/
COPY --from=builder /opt/texmacs/src/TeXmacs/fonts /usr/local/share/TeXmacs/fonts/
COPY --from=builder /opt/texmacs/src/TeXmacs/plugins /usr/local/share/TeXmacs/plugins
COPY --from=builder /opt/texmacs/src/TeXmacs/styles /usr/local/share/TeXmacs/styles/
COPY --from=builder /opt/texmacs/src/TeXmacs/texts /usr/local/share/TeXmacs/texts/
COPY --from=builder /opt/texmacs/src/TeXmacs/langs /usr/local/share/TeXmacs/langs/
COPY --from=builder /opt/texmacs/src/TeXmacs/packages /usr/local/share/TeXmacs/packages/
COPY --from=builder /opt/texmacs/src/TeXmacs/bin/texmacs.bin /usr/local/libexec/TeXmacs/bin/

COPY --from=builder /opt/texmacs/src/misc/scripts/texmacs /usr/local/bin/
COPY --from=builder /opt/texmacs/src/LICENSE /usr/local/share/TeXmacs/LICENSE
COPY --from=builder /opt/texmacs/src/misc/docker/docker-entrypoint.sh /usr/local/bin/
COPY --from=builder /opt/texmacs/src/misc/docker/server-preferences.scm /srv/texmacs-server/.TeXmacs/system/preferences.scm
COPY --from=builder /opt/texmacs/src/misc/docker/mail.rc /etc/mail.rc
COPY --from=builder /opt/texmacs/src/misc/docker/license.tm /srv/texmacs-server/.TeXmacs/server/license.tm

RUN set -eux; \
	chmod +x /usr/local/bin/texmacs; \
	chmod +x /usr/local/bin/docker-entrypoint.sh

# Set environment variables
#ENV TEXMACS_PATH=/usr/local/share/TeXmacs
#ENV TEXMACS_BIN_PATH=/usr/local/libexec/TeXmacs
#ENV PATH="/usr/local/libexec/TeXmacs/bin:${PATH}"
#ENV QT_DEBUG_PLUGINS=1

RUN set -eux; \
	chown -R texmacs-server:texmacs-server \
		/srv/texmacs-server/;

ENTRYPOINT [ "/usr/local/bin/docker-entrypoint.sh" ]

EXPOSE 6561
CMD [ "texmacs" ]
