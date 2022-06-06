MAKEINFOFLAGS = --enable-encoding -I $(srcdir)/../en -I ..

TEXINFO_TEX = ../texinfo.tex

lang = pt_BR
langsdir = /pt_BR

include ../common-lang.mk

maxima_TEXINFOS = $(wildcard *.texi)

EXTRA_DIST += include-maxima.texi.in $(maxima_TEXINFOS) $(wildcard *.info*)

