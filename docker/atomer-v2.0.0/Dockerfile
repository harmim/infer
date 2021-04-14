FROM debian:buster-slim

COPY .bashrc /root/.bashrc
COPY .inputrc /root/.inputrc
COPY .vimrc /root/.vimrc
COPY .vim /root/.vim

RUN apt-get update
# mkdir the man/man1 directory due to Debian bug #863199
RUN mkdir -p /usr/share/man/man1
RUN apt-get install --yes --no-install-recommends \
	autoconf \
	automake \
	bubblewrap \
	bzip2 \
	cmake \
	curl \
	g++ \
	gcc \
	git \
	libc6-dev \
	libgmp-dev \
	libmpfr-dev \
	libsqlite3-dev \
	sqlite3 \
	make \
	openjdk-11-jdk-headless \
	patch \
	patchelf \
	pkg-config \
	python3.7 \
	python3-distutils \
	unzip \
	xz-utils \
	zlib1g-dev \
	vim \
	man
RUN rm -rf /var/lib/apt/lists/*

# install opam 2
RUN curl -sL https://github.com/ocaml/opam/releases/download/2.0.6/opam-2.0.6-x86_64-linux > /usr/bin/opam && \
	chmod +x /usr/bin/opam
# disable sandboxing
# Without this opam fails to compile OCaml for some reason. We don't need sandboxing inside a Docker container anyway.
RUN opam init --reinit --bare --disable-sandboxing --yes --auto-setup

# download Infer
RUN cd / && \
	git clone --depth 1 --recurse-submodules https://github.com/harmim/infer && \
	cd /infer && \
	git checkout atomicity-sets

# Build opam deps first, then clang, then infer. This way if any step
# fails we don't lose the significant amount of work done in the
# previous steps.
RUN cd /infer && ./build-infer.sh --only-setup-opam
RUN cd /infer && \
	eval $(opam env) && \
	./autogen.sh && \
	./configure && \
	./facebook-clang-plugins/clang/src/prepare_clang_src.sh && \
	./facebook-clang-plugins/clang/setup.sh
RUN cd /infer && make

# install Infer
RUN cd /infer && make install
ENV INFER_HOME /infer/infer
ENV PATH ${INFER_HOME}/bin:${PATH}
ENV MANPATH ${INFER_HOME}/man:${MANPATH}

# generate a release
RUN cd /infer && \
	make install-with-libs \
	BUILD_MODE=opt \
	PATCHELF=patchelf \
	DESTDIR="/infer-release" \
	libdir_relative_to_bindir="../lib"
RUN tar -cJf /infer-atomer-v2.0.0.tar.xz -C /infer-release/usr/local . && \
	rm -rf /infer-release
