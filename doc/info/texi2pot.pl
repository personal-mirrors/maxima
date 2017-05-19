#!/usr/bin/perl

print "# This file contains all translatable strings from a .texi file.\\n\n";
print "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n";
print "# This file is distributed under the same license as the PACKAGE package.\n";
print "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n";
print "#\n";
print "#, fuzzy\n";
print "msgid \"\"\n";
print "\"Project-Id-Version: PACKAGE VERSION\\n\"\n";
print "\"Report-Msgid-Bugs-To: \\n\"\n";
print "\"POT-Creation-Date: 2017-05-06 22:01+0200\\n\"\n";
print "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n";
print "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n";
print "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n";
print "\"Language: \\n\"\n";
print "\"MIME-Version: 1.0\\n\"\n";
print "\"Content-Type: text/plain; charset=UTF-8\\n\"\n";
print "\"Content-Transfer-Encoding: 8bit\\n\"\n";

$lastline = "";
$paragraph = "";

# Read the file line by line
while (my $line = <>) {

    # Skip duplicate blank lines
    if (($line ne "\n") || ($line ne $lastline))
    {
	# Is this the last line of a paragraph?
	if($line eq "\n")
	{
	    if ($paragraph ne "")
	    {
		$message  = "msgstr \"\"\n\n";
		$message .= "#: ";
		$message .= $ARGV;
		$message .=":";
		$message .= $.;
		$message .= "\nmsgid \"\"\n";
		$message .= $paragraph;
		print $message;
	    }
	    $paragraph = "";
	}
	else
	{
	    # Escape quotation marks
	    $line =~ s/\"/\\\\"/g;
	    # Add a c-style newline marker to the line
	    $line =~ s/(..*)/\"\1\\n\"/g;
	    $paragraph .= $line;
	}
    }
    $lastline = $line;
}

print "msgstr \"\"\n\n";
