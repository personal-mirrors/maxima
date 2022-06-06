MAKEINFOFLAGS = --enable-encoding -I $(srcdir)/../en -I ..

TEXINFO_TEX = ../texinfo.tex

lang = es
langsdir = /es

include ../common-lang.mk

maxima_TEXINFOS = $(wildcard *.texi)

EXTRA_DIST += txi-es_ES.tex include-maxima.texi.in $(maxima_TEXINFOS) $(wildcard *.info*)
