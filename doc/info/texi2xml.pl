#!/usr/bin/perl

# texi2xml: Convert a .texi file into a XML file accessible to itstool
# Copyright (C) {2018}  {Gunter Königsmann}

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# SPDX-License-Identifier: GPL-3.0+

print "<?xml version=\"1.0\"?>\n";
print "<!DOCTYPE texifile [\n";
print "<!ELEMENT texifile (paragraph)>\n";
print "<!ELEMENT paragraph (#PCDATA)>\n";
print "]>\n";
print "<texifile>\n";
    
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
	  print "<paragraph space=\"preserve\">";
	  print $paragraph;
	  print "</paragraph>\n";
	  $paragraph = "";
	}
	else
	{
	    # Excape spacial chars for XML
	    $line =~ s/&/&amp;/g;
	    $line =~ s/</&lt;/g;
	    $line =~ s/>/&gt;/g;
	    $paragraph .= $line;
	}
    }
    $lastline = $line;
}

if($paragraph ne "")
{
    print "<paragraph space=\"preserve\">";
    print $paragraph;
    print "</paragraph>\n";
    $paragraph = "";
}

print "</texifile>\n";
