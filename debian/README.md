This is a debian recipe that (hopefully) creates a debian package of
the jupyter front-end for maxima. Just copy the debian folder into
the directory with the contents of the tarball and type

    debuild

In order to make this work debuild needs to have access to the
original tarball. But it will tell where and under which name it
expects it. The e-mail-address the .deb packages will be gpg-signed
with and the version of the program are taken from the file
debian/changelog. Which means that the changelog isn't just
documentation but a vital file.

The rest of the more important files are:


debian/rules
============

contains the info how the program is built and installed


debian/control
==============

declares the dependencies of the program, where to obtain it from
and most of the metadata.


debian/watch
============

Where to automatically download the file from and where to look
in order to look if this is the latest upstream version.


debian/upstream/signing-key.asc
===============================

A minimal export of key the release is signed with, see
https://wiki.debian.org/Creating%20signed%20GitHub%20releases


debian/upstream/metadata
========================

Can be used in order to create a bibtex reference.
