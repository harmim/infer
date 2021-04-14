FROM debian:buster-slim

COPY .bashrc /root/.bashrc
COPY .inputrc /root/.inputrc
COPY .vimrc /root/.vimrc
COPY .vim /root/.vim

RUN apt-get update
# mkdir the man/man1 directory due to Debian bug #863199
RUN mkdir -p /usr/share/man/man1
RUN apt-get install --yes --no-install-recommends \
	curl \
	g++ \
	gcc \
	git \
	sqlite3 \
	make \
	openjdk-11-jdk-headless \
	xz-utils \
	vim \
	man
RUN rm -rf /var/lib/apt/lists/*

# download Infer
RUN mkdir /infer && \
	curl -sL https://github.com/harmim/infer/releases/download/atomer-v2.0.0/infer-atomer-v2.0.0.tar.xz \
	| tar -xJ -C /infer

# installl infer
ENV INFER_HOME /infer
ENV PATH ${INFER_HOME}/bin:${PATH}
ENV MANPATH ${INFER_HOME}/share/man:${MANPATH}
