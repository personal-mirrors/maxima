MAKEINFOFLAGS = --enable-encoding -I $(srcdir)/../en -I .. --force -e 20

TEXINFO_TEX = ../texinfo.tex

lang = ja
langsdir = /ja

include ../common-lang.mk

maxima_TEXINFOS = $(wildcard *.texi)

EXTRA_DIST += include-maxima.texi.in $(maxima_TEXINFOS) $(wildcard *.info*)
