#!/bin/sh
cat > $2 <<EOF
# SOME DESCRIPTIVE TITLE.">$texname
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"Report-Msgid-Bugs-To: \n"
"POT-Creation-Date: 2017-05-06 22:01+0200\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
 
EOF
echo "# $1">>$2
echo "msgid \"\"">>$2

sed '
    N;
    /^\n$/d;
    P;
    D
' $1| sed -e "s/\\\"/\\\\\\\"/g"| sed -e "s/\(..*\)/\"\1\\\\n\"/g" | sed -e "s/^$/msgstr \"\"\n\n# $1\nmsgid \"\"/g">>$2
echo "msgstr \"\"">>$2
