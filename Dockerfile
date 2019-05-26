# If we start with a more recent debian version we depend on a glibc that is at
# least as new as the one shipped with this version excluding users of
# debian-oldstable
FROM ubuntu:trusty
#FROM debian:oldstable

ARG ARCH=x86_64

RUN apt-get update && apt-get -y install git build-essentials autoconf python binutils \
    texinfo gcc libtool desktop-file-utils sbcl clisp ecl libffi-dev gcl texinfo libreadline-dev locales automake debhelper autoconf gawk texlive-latex-recommended texlive-latex-base python debhelper  gnuplot

RUN git clone https://git.code.sf.net/p/maxima/code maxima-code
RUN cd maxima-code&&git checkout DebianPackaging
RUN cd maxima-code&&uscan
RUN cd maxima-code&&debuild
