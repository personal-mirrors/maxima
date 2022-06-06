MAKEINFOFLAGS = --enable-encoding -I $(srcdir)/../en -I ..

TEXINFO_TEX = ../texinfo.tex
lang = de
langsdir = /de

include ../common-lang.mk

maxima_TEXINFOS = $(wildcard *.texi)

EXTRA_DIST += manual.css include-maxima.texi.in $(maxima_TEXINFOS) $(wildcard *.info*)
