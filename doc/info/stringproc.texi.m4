@c -*- Mode: texinfo -*-
@menu
* Introduction to String Processing::
* Input and Output::
* Characters::
* String Processing::
* Octets and Utilities for Cryptography::
@end menu

@c -----------------------------------------------------------------------------
@c -----------------------------------------------------------------------------
@node Introduction to String Processing, Input and Output, stringproc-pkg, stringproc-pkg
@section Introduction to String Processing

The package @code{stringproc} contains functions for processing strings 
and characters including formatting, encoding and data streams. 
This package is completed by some tools for cryptography, e.g. base64 and hash 
functions.

It can be directly loaded via @code{load(stringproc)} or automatically by 
using one of its functions.

For questions and bug reports please contact the author. The following 
command prints his e-mail-address.

@code{printf(true, "~@{~a~@}@@gmail.com", split(sdowncase("Volker van Nek")))$}


A string is constructed by typing e.g. @code{"Text"}. 
When the option variable @mref{stringdisp} is set to @code{false}, which is 
the default, the double quotes won't be printed. 
@ref{stringp} is a test, if an object is a string.

@example
(%i1) str: "Text";
(%o1)                         Text
(%i2) stringp(str);
(%o2)                         true
@end example

Characters are represented by a string of length 1. 
@ref{charp} is the corresponding test.

@example
(%i1) char: "e";
(%o1)                           e
(%i2) charp(char);
(%o2)                         true
@end example

In Maxima position indices in strings are like in list 1-indexed 
which results to the following consistency.

@example
(%i1) is(charat("Lisp",1) = charlist("Lisp")[1]);
(%o1)                         true
@end example

A string may contain Maxima expressions. 
These can be parsed with @ref{parse_string}.

@example
(%i1) map(parse_string, ["42" ,"sqrt(2)", "%pi"]);
(%o1)                   [42, sqrt(2), %pi]
(%i2) map('float, %);
(%o2)        [42.0, 1.414213562373095, 3.141592653589793]
@end example

Strings can be processed as characters or in binary form as octets. 
Functions for conversions are @ref{string_to_octets} and @ref{octets_to_string}.
Usable encodings depend on the platform, the application and the 
underlying Lisp.
(The following shows Maxima in GNU/Linux, compiled with SBCL.)

@example
(%i1) obase: 16.$
(%i2) string_to_octets("$@pounds{}@euro{}", "cp1252");
(%o2)                     [24, 0A3, 80]
(%i3) string_to_octets("$@pounds{}@euro{}", "utf-8");
(%o3)               [24, 0C2, 0A3, 0E2, 82, 0AC]
@end example

Strings may be written to character streams or as octets to binary streams. 
The following example demonstrates file in and output of characters.

@ref{openw} returns an output stream to a file, 
@ref{printf} writes formatted to that file and by e.g. 
@ref{close} all characters contained in the stream are written to the file.

@example
(%i1) s: openw("file.txt");
(%o1)                #<output stream file.txt>
(%i2) printf(s, "~%~d ~f ~a ~a ~f ~e ~a~%", 
42, 1.234, sqrt(2), %pi, 1.0e-2, 1.0e-2, 1.0b-2)$
(%i3) close(s)$
@end example

@ref{openr} then returns an input stream from the previously used file and 
@ref{readline} returns the line read as a string.
The string may be tokenized by e.g. @ref{split} or @ref{tokens} and 
finally parsed by @ref{parse_string}.

@example
(%i4) s: openr("file.txt");
(%o4)                 #<input stream file.txt>
(%i5) readline(s);
(%o5)          42 1.234 sqrt(2) %pi 0.01 1.0E-2 1.0b-2
(%i6) map(parse_string, split(%));
(%o6)       [42, 1.234, sqrt(2), %pi, 0.01, 0.01, 1.0b-2]
(%i7) close(s)$
@end example

@opencatbox
@category{Strings}
@category{Share packages}
@category{Package stringproc}
@closecatbox


@c -----------------------------------------------------------------------------
@c -----------------------------------------------------------------------------
@node Input and Output, Characters, Introduction to String Processing, stringproc-pkg
@section Input and Output

Example: Formatted printing to a file.

@example
(%i1) s: openw("file.txt");
(%o1)                      #<output stream file.txt>
(%i2) control: 
"~2tAn atom: ~20t~a~%~2tand a list: ~20t~@{~r ~@}~%~2t\
and an integer: ~20t~d~%"$
(%i3) printf( s,control, 'true,[1,2,3],42 )$
(%o3)                                false
(%i4) close(s);
(%o4)                                true
(%i5) s: openr("file.txt");
(%o5)                      #<input stream file.txt>
(%i6) while stringp( tmp:readline(s) ) do print(tmp)$
  An atom:          true 
  and a list:       one two three  
  and an integer:   42 
(%i7) close(s)$
@end example

@c -----------------------------------------------------------------------------
@anchor{close}
@deffn {Function} close (@var{stream}) 

Closes @var{stream} and returns @code{true} if @var{stream} had been open. 

@opencatbox
@category{File input}
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{flength}
@deffn {Function} flength (@var{stream})

@var{stream} has to be an open stream from or to a file. 
@code{flength} then returns the number of bytes which are currently present in this file.

Example: See @ref{writebyte} .

@opencatbox
@category{File input}
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{flush_output}
@deffn {Function} flush_output (@var{stream})

Flushes @var{stream} where @var{stream} has to be an output stream to a file. 

Example: See @ref{writebyte} .

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{fposition}
@deffn {Function} fposition @
@fname{fposition} (@var{stream}) @
@fname{fposition} (@var{stream}, @var{pos})

Returns the current position in @var{stream}, if @var{pos} is not used. 
If @var{pos} is used, @code{fposition} sets the position in @var{stream}.
@var{stream} has to be a stream from or to a file and 
@var{pos} has to be a positive number.

Positions in data streams are like in strings or lists 1-indexed, 
i.e. the first element in @var{stream} is in position 1.

@opencatbox
@category{File input}
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{freshline}
@deffn {Function} freshline @
@fname{freshline} ()  @
@fname{freshline} (@var{stream}) 

Writes a new line to the standard output stream 
if the position is not at the beginning of a line und returns @code{true}.
Using the optional argument @var{stream} the new line is written to that stream. 
There are some cases, where @code{freshline()} does not work as expected. 

See also @ref{newline}.

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{get_output_stream_string}
@deffn {Function} get_output_stream_string (@var{stream})

Returns a string containing all the characters currently present in 
@var{stream} which must be an open string-output stream. 
The returned characters are removed from @var{stream}.

Example: See @ref{make_string_output_stream} .

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{make_string_input_stream}
@deffn {Function} make_string_input_stream @
@fname{make_string_input_stream} (@var{string}) @
@fname{make_string_input_stream} (@var{string}, @var{start}) @
@fname{make_string_input_stream} (@var{string}, @var{start}, @var{end})

Returns an input stream which contains parts of @var{string} and an end of file. 
Without optional arguments the stream contains the entire string 
and is positioned in front of the first character. 
@var{start} and @var{end} define the substring contained in the stream. 
The first character is available at position 1.
 
@example
(%i1) istream : make_string_input_stream("text", 1, 4);
(%o1)              #<string-input stream from "text">
(%i2) (while (c : readchar(istream)) # false do sprint(c), newline())$
t e x 
(%i3) close(istream)$
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{make_string_output_stream}
@deffn {Function} make_string_output_stream ()

Returns an output stream that accepts characters. Characters currently present 
in this stream can be retrieved by @ref{get_output_stream_string}.
 
@example
(%i1) ostream : make_string_output_stream();
(%o1)               #<string-output stream 09622ea0>
(%i2) printf(ostream, "foo")$

(%i3) printf(ostream, "bar")$

(%i4) string : get_output_stream_string(ostream);
(%o4)                            foobar
(%i5) printf(ostream, "baz")$

(%i6) string : get_output_stream_string(ostream);
(%o6)                              baz
(%i7) close(ostream)$
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{newline}
@deffn {Function} newline @
@fname{newline} ()  @
@fname{newline} (@var{stream}) 

Writes a new line to the standard output stream. 
Using the optional argument @var{stream} the new line is written to that stream. 
There are some cases, where @code{newline()} does not work as expected. 

See @ref{sprint} for an example of using @code{newline()}.

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{opena}
@deffn {Function} opena (@var{file}) 

Returns a character output stream to @var{file}.
If an existing file is opened, @code{opena} appends elements at the end of @var{file}.

For binary output see @ref{Functions and Variables for binary input and output, , opena_binary} .

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{openr}
@deffn {Function} openr @
@fname{openr} (@var{file}) @
@fname{openr} (@var{file}, @var{encoding}) 


Returns a character input stream to @var{file}.
@code{openr} assumes that @var{file} already exists.
If reading the file results in a lisp error about its encoding
passing the correct string as the argument @var{encoding} might help.
The available encodings and their names depend on the lisp being used.
For sbcl a list of suitable strings can be found at
@url{http://www.sbcl.org/manual/#External-Formats}.

For binary input see @ref{Functions and Variables for binary input and output, , openr_binary} .
See also @mref{close} and @mrefdot{openw}

@example
(%i1) istream : openr("data.txt","EUC-JP");
(%o1)     #<FD-STREAM for "file /home/gunter/data.txt" @{10099A3AE3@}>
(%i2) close(istream);
(%o2)                                true
@end example


@opencatbox
@category{File input}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{openw}
@deffn {Function} openw (@var{file}) 

Returns a character output stream to @var{file}.
If @var{file} does not exist, it will be created.
If an existing file is opened, @code{openw} destructively modifies @var{file}.

For binary output see @ref{Functions and Variables for binary input and output, , openw_binary} .

See also @mref{close} and @mrefdot{openr}

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{printf}
@deffn {Function} printf @
@fname{printf} (@var{dest}, @var{string}) @
@fname{printf} (@var{dest}, @var{string}, @var{expr_1}, ..., @var{expr_n})

Produces formatted output by outputting the characters of control-string 
@var{string} and observing that a tilde introduces a directive.
The character after the tilde, possibly preceded by prefix parameters 
and modifiers, specifies what kind of formatting is desired.
Most directives use one or more elements of the arguments 
@var{expr_1}, ..., @var{expr_n} to create their output.

If @var{dest} is a stream or @code{true}, then @code{printf} returns @code{false}.
Otherwise, @code{printf} returns a string containing the output.
By default the streams @var{stdin}, @var{stdout} and @var{stderr} are defined.
If maxima is running as a server (which is the normal case if maxima communicating
with a graphical user interface) @code{setup-client} will define @var{old_stdout} and
@var{old_stderr}, too.

@code{printf} provides the Common Lisp function @code{format} in Maxima. 
The following example illustrates the general relation between these two 
functions.

@example
(%i1) printf(true, "R~dD~d~%", 2, 2);
R2D2
(%o1)                                false
(%i2) :lisp (format t "R~dD~d~%" 2 2)
R2D2
NIL
@end example

The following description is limited to a rough sketch of the possibilities of 
@code{printf}.
The Lisp function @code{format} is described in detail in many reference books. 
Of good help is e.g. the free available online-manual 
"Common Lisp the Language" by Guy L. Steele. See chapter 22.3.3 there. 

@example
   ~%       new line
   ~&       fresh line
   ~t       tab
   ~$       monetary
   ~d       decimal integer
   ~b       binary integer
   ~o       octal integer
   ~x       hexadecimal integer
   ~br      base-b integer
   ~r       spell an integer
   ~p       plural
   ~f       floating point
   ~e       scientific notation
   ~g       ~f or ~e, depending upon magnitude
   ~h       bigfloat
   ~a       uses Maxima function string
   ~s       like ~a, but output enclosed in "double quotes"
   ~~       ~
   ~<       justification, ~> terminates
   ~(       case conversion, ~) terminates 
   ~[       selection, ~] terminates 
   ~@{       iteration, ~@} terminates
@end example

The directive ~h for bigfloat is no Lisp-standard and is therefore illustrated below. 

Note that the directive ~* is not supported.

If @var{dest} is a stream or @code{true}, then @code{printf} returns @code{false}.
Otherwise, @code{printf} returns a string containing the output.

@example
(%i1) printf( false, "~a ~a ~4f ~a ~@@r", 
              "String",sym,bound,sqrt(12),144), bound = 1.234;
(%o1)                 String sym 1.23 2*sqrt(3) CXLIV
(%i2) printf( false,"~@{~a ~@}",["one",2,"THREE"] );
(%o2)                          one 2 THREE 
(%i3) printf(true,"~@{~@{~9,1f ~@}~%~@}",mat ),
          mat = args(matrix([1.1,2,3.33],[4,5,6],[7,8.88,9]))$
      1.1       2.0       3.3 
      4.0       5.0       6.0 
      7.0       8.9       9.0 
(%i4) control: "~:(~r~) bird~p ~[is~;are~] singing."$
(%i5) printf( false,control, n,n,if n=1 then 1 else 2 ), n=2;
(%o5)                    Two birds are singing.
@end example

The directive ~h has been introduced to handle bigfloats. 

@example
~w,d,e,x,o,p@@H
 w : width
 d : decimal digits behind floating point
 e : minimal exponent digits
 x : preferred exponent
 o : overflow character
 p : padding character
 @@ : display sign for positive numbers
@end example

@example
(%i1) fpprec : 1000$
(%i2) printf(true, "|~h|~%", 2.b0^-64)$
|0.0000000000000000000542101086242752217003726400434970855712890625|
(%i3) fpprec : 26$
(%i4) printf(true, "|~h|~%", sqrt(2))$
|1.4142135623730950488016887|
(%i5) fpprec : 24$
(%i6) printf(true, "|~h|~%", sqrt(2))$
|1.41421356237309504880169|
(%i7) printf(true, "|~28h|~%", sqrt(2))$
|   1.41421356237309504880169|
(%i8) printf(true, "|~28,,,,,'*h|~%", sqrt(2))$
|***1.41421356237309504880169|
(%i9) printf(true, "|~,18h|~%", sqrt(2))$
|1.414213562373095049|
(%i10) printf(true, "|~,,,-3h|~%", sqrt(2))$
|1414.21356237309504880169b-3|
(%i11) printf(true, "|~,,2,-3h|~%", sqrt(2))$
|1414.21356237309504880169b-03|
(%i12) printf(true, "|~20h|~%", sqrt(2))$
|1.41421356237309504880169|
(%i13) printf(true, "|~20,,,,'+h|~%", sqrt(2))$
|++++++++++++++++++++|
@end example

For conversion of objects to strings also see @mrefcomma{concat} @mrefcomma{sconcat}
@mref{string} and @mrefdot{simplode}

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{readbyte}
@deffn {Function} readbyte (@var{stream})

Removes and returns the first byte in @var{stream} which must be a binary input stream. 
If the end of file is encountered @code{readbyte} returns @code{false}.

Example: Read the first 16 bytes from a file encrypted with AES in OpenSSL. 

@example
(%i1) ibase: obase: 16.$

(%i2) in: openr_binary("msg.bin");
(%o2)                       #<input stream msg.bin>
(%i3) (L:[],  thru 16. do push(readbyte(in), L),  L:reverse(L));
(%o3) [53, 61, 6C, 74, 65, 64, 5F, 5F, 88, 56, 0DE, 8A, 74, 0FD, 0AD, 0F0]
(%i4) close(in);
(%o4)                                true
(%i5) map(ascii, rest(L,-8));
(%o5)                      [S, a, l, t, e, d, _, _]
(%i6) salt: octets_to_number(rest(L,8));
(%o6)                          8856de8a74fdadf0
@end example

@opencatbox
@category{File input}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{readchar}
@deffn {Function} readchar (@var{stream})

Removes and returns the first character in @var{stream}. 
If the end of file is encountered @code{readchar} returns @code{false}.

Example: See @ref{make_string_input_stream}.

@opencatbox
@category{File input}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{readline}
@deffn {Function} readline (@var{stream}) 

Returns a string containing all characters starting at the current position 
in @var{stream} up to the end of the line or @code{false} 
if the end of the file is encountered.

@opencatbox
@category{File input}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sprint}
@deffn {Function} sprint (@var{expr_1}, @dots{}, @var{expr_n})

Evaluates and displays its arguments one after the other `on a line' starting at
the leftmost position.  The expressions are printed with a space character right next 
to the number, and it disregards line length.  
@code{newline()} might be used for line breaking.

Example: Sequential printing with @code{sprint}. 
Creating a new line with @code{newline()}.

@example
(%i1) for n:0 thru 19 do sprint(fib(n))$
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181
(%i2) for n:0 thru 22 do ( 
         sprint(fib(n)), 
         if mod(n,10) = 9 then newline() )$
0 1 1 2 3 5 8 13 21 34 
55 89 144 233 377 610 987 1597 2584 4181 
6765 10946 17711 
@end example

@opencatbox
@category{Package stringproc}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{writebyte}
@deffn {Function} writebyte (@var{byte}, @var{stream})

Writes @var{byte} to @var{stream} which must be a binary output stream. 
@code{writebyte} returns @code{byte}.

Example: Write some bytes to a binary file output stream. 
In this example all bytes correspond to printable characters and are printed 
by @code{printfile}. 
The bytes remain in the stream until @code{flush_output} or @code{close} have been called.

@example
(%i1) ibase: obase: 16.$

(%i2) bytes: map(cint, charlist("GNU/Linux"));
(%o2)                [47, 4E, 55, 2F, 4C, 69, 6E, 75, 78]
(%i3) out: openw_binary("test.bin");
(%o3)                      #<output stream test.bin>
(%i4) for i thru 3 do writebyte(bytes[i], out);
(%o4)                                done
(%i5) printfile("test.bin")$

(%i6) flength(out);
(%o6)                                  0
(%i7) flush_output(out);
(%o7)                                true
(%i8) flength(out);
(%o8)                                  3
(%i9) printfile("test.bin")$
GNU
(%i0A) for b in rest(bytes,3) do writebyte(b, out);
(%o0A)                               done
(%i0B) close(out);
(%o0B)                               true
(%i0C) printfile("test.bin")$
GNU/Linux
@end example

@opencatbox
@category{File output}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@c -----------------------------------------------------------------------------
@node Characters, String Processing, Input and Output, stringproc-pkg
@section Characters

Characters are strings of length 1.

@c -----------------------------------------------------------------------------
@anchor{adjust_external_format}
@deffn {Function} adjust_external_format () 

Prints information about the current external format of the Lisp reader 
and in case the external format encoding differs from the encoding of the 
application which runs Maxima @code{adjust_external_format} tries to adjust 
the encoding or prints some help or instruction.
@code{adjust_external_format} returns @code{true} when the external format has 
been changed and @code{false} otherwise.

Functions like @ref{cint}, @ref{unicode}, @ref{octets_to_string} 
and @ref{string_to_octets} need UTF-8 as the external format of the 
Lisp reader to work properly over the full range of Unicode characters. 

Examples (Maxima on Windows, March 2016): 
Using @code{adjust_external_format} when the default external format 
is not equal to the encoding provided by the application.

1. Command line Maxima

In case a terminal session is preferred it is recommended to use Maxima compiled 
with SBCL. Here Unicode support is provided by default and calls to 
@code{adjust_external_format} are unnecessary. 

If Maxima is compiled with CLISP or GCL it is recommended to change 
the terminal encoding from CP850 to CP1252. 
@code{adjust_external_format} prints some help. 

CCL reads UTF-8 while the terminal input is CP850 by default. 
CP1252 is not supported by CCL. @code{adjust_external_format} 
prints instructions for changing the terminal encoding and external format 
both to iso-8859-1.

2. wxMaxima

In wxMaxima SBCL reads CP1252 by default but the input from the application 
is UTF-8 encoded. Adjustment is needed. 

Calling @code{adjust_external_format} and restarting Maxima 
permanently changes the default external format to UTF-8.

@example
(%i1)adjust_external_format();
The line
(setf sb-impl::*default-external-format* :utf-8)
has been appended to the init file
C:/Users/Username/.sbclrc
Please restart Maxima to set the external format to UTF-8.
(%i1) false
@end example

Restarting Maxima.

@example
(%i1) adjust_external_format();
The external format is currently UTF-8
and has not been changed.
(%i1) false
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{alphacharp}
@deffn {Function} alphacharp (@var{char})  
  
Returns @code{true} if @var{char} is an alphabetic character. 

To identify a non-US-ASCII character as an alphabetic character 
the underlying Lisp must provide full Unicode support. 
E.g. a German umlaut is detected as an alphabetic character with SBCL in GNU/Linux 
but not with GCL. 
(In Windows Maxima, when compiled with SBCL, must be set to UTF-8. 
See @ref{adjust_external_format} for more.) 

Example: Examination of non-US-ASCII characters.

The underlying Lisp (SBCL, GNU/Linux) is able to convert the typed character 
into a Lisp character and to examine.

@example
(%i1) alphacharp("@"u");
(%o1)                          true
@end example

In GCL this is not possible. An error break occurs.

@example
(%i1) alphacharp("u");
(%o1)                          true
(%i2) alphacharp("@"u");

package stringproc: @"u cannot be converted into a Lisp character.
 -- an error.
@end example

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{alphanumericp}
@deffn {Function} alphanumericp (@var{char}) 

Returns @code{true} if @var{char} is an alphabetic character or a digit 
(only corresponding US-ASCII characters are regarded as digits). 

Note: See remarks on @ref{alphacharp}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{ascii}
@deffn {Function} ascii (@var{int}) 

Returns the US-ASCII character corresponding to the integer @var{int}
which has to be less than @code{128}.

See @ref{unicode} for converting code points larger than @code{127}.

Examples:

@example
(%i1) for n from 0 thru 127 do ( 
        ch: ascii(n), 
        if alphacharp(ch) then sprint(ch),
        if n = 96 then newline() )$
A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 
a b c d e f g h i j k l m n o p q r s t u v w x y z
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{cequal}
@deffn {Function} cequal (@var{char_1}, @var{char_2}) 
         
Returns @code{true} if @var{char_1} and @var{char_2} are the same character. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{cequalignore}
@deffn {Function} cequalignore (@var{char_1}, @var{char_2}) 
  
Like @code{cequal} but ignores case which is only possible for non-US-ASCII 
characters when the underlying Lisp is able to recognize a character as an 
alphabetic character. See remarks on @ref{alphacharp}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{cgreaterp}
@deffn {Function} cgreaterp (@var{char_1}, @var{char_2}) 
  
Returns @code{true} if the code point of @var{char_1} is greater than the 
code point of @var{char_2}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{cgreaterpignore}
@deffn {Function} cgreaterpignore (@var{char_1}, @var{char_2}) 

Like @code{cgreaterp} but ignores case which is only possible for non-US-ASCII 
characters when the underlying Lisp is able to recognize a character as an 
alphabetic character. See remarks on @ref{alphacharp}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{charp}
@deffn {Function} charp (@var{obj}) 

Returns @code{true} if @var{obj} is a Maxima-character.
See introduction for example.

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{cint}
@deffn {Function} cint (@var{char}) 

Returns the Unicode code point of @var{char} which must be a 
Maxima character, i.e. a string of length @code{1}.

Examples: The hexadecimal code point of some characters 
(Maxima with SBCL on GNU/Linux). 

@example
(%i1) obase: 16.$
(%i2) map(cint, ["$","@pounds{}","@euro{}"]);
(%o2)                           [24, 0A3, 20AC]
@end example

Warning: It is not possible to enter characters corresponding to code points 
larger than 16 bit in wxMaxima with SBCL on Windows when the external format 
has not been set to UTF-8. See @ref{adjust_external_format}.

@c Command @U not supported by texinfo 5.
@c @example
@c (%i3) cint("@U{1d538}");
@c (%o3)                                1D538
@c @end example

CMUCL doesn't process these characters as one character. 
@code{cint} then returns @code{false}. 
@c Converting to UTF-8-octets and finally to Unicode serves as a workaround.
Converting a character to a code point via UTF-8-octets may serve as a workaround: 

@code{utf8_to_unicode(string_to_octets(character));}

@c Command @U not supported by texinfo 5.
@c @example
@c (%i4) utf8_to_unicode(string_to_octets("@U{1d538}"));
@c (%o4)                                1D538
@c @end example

See @ref{utf8_to_unicode}, @ref{string_to_octets}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{clessp}
@deffn {Function} clessp (@var{char_1}, @var{char_2})

Returns @code{true} if the code point of @var{char_1} is less than the 
code point of @var{char_2}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{clesspignore}
@deffn {Function} clesspignore (@var{char_1}, @var{char_2})

Like @code{clessp} but ignores case which is only possible for non-US-ASCII 
characters when the underlying Lisp is able to recognize a character as an 
alphabetic character. See remarks on @ref{alphacharp}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{constituent}
@deffn {Function} constituent (@var{char}) 

Returns @code{true} if @var{char} is a graphic character but not a space character.
A graphic character is a character one can see, plus the space character.
(@code{constituent} is defined by Paul Graham. 
See Paul Graham, ANSI Common Lisp, 1996, page 67.)

@example
(%i1) for n from 0 thru 255 do ( 
tmp: ascii(n), if constituent(tmp) then sprint(tmp) )$
! " #  %  ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @@ A B
C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ ` a b c
d e f g h i j k l m n o p q r s t u v w x y z @{ | @} ~
@end example

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@c @deffn {Function} cunlisp (@var{lisp_char}) 
@c Converts a Lisp-character into a Maxima-character.
@c (You won't need it.)
@c 
@c @opencatbox
@c @category{Package stringproc}
@c @closecatbox
@c 
@c @end deffn

@c -----------------------------------------------------------------------------
@anchor{digitcharp}
@deffn {Function} digitcharp (@var{char}) 
  
Returns @code{true} if @var{char} is a digit where only the corresponding 
US-ASCII-character is regarded as a digit.

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@c @deffn {Function} lcharp (@var{obj}) 
@c Returns @code{true} if @var{obj} is a Lisp-character.
@c (You won't need it.)
@c 
@c @opencatbox
@c @category{Predicate functions}
@c @category{Package stringproc}
@c @closecatbox
@c 
@c @end deffn

@c -----------------------------------------------------------------------------
@anchor{lowercasep}
@deffn {Function} lowercasep (@var{char}) 
   
Returns @code{true} if @var{char} is a lowercase character. 

Note: See remarks on @ref{alphacharp}.

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{newline_variable}
@defvr {Variable} newline 

The newline character (ASCII-character 10). 

@opencatbox
@category{Global variables}
@category{Package stringproc}
@closecatbox

@end defvr

@c -----------------------------------------------------------------------------
@anchor{space_variable}
@defvr {Variable} space   

The space character.

@opencatbox
@category{Global variables}
@category{Package stringproc}
@closecatbox

@end defvr

@c -----------------------------------------------------------------------------
@anchor{tab_variable}
@defvr {Variable} tab     

The tab character.

@opencatbox
@category{Global variables}
@category{Package stringproc}
@closecatbox

@end defvr

@c -----------------------------------------------------------------------------
@anchor{unicode}
@deffn {Function} unicode (@var{arg}) 

Returns the character defined by @var{arg} which might be a Unicode code point 
or a name string if the underlying Lisp provides full Unicode support. 

Example: Characters defined by hexadecimal code points
(Maxima with SBCL on GNU/Linux). 

@example
(%i1) ibase: 16.$
(%i2) map(unicode, [24, 0A3, 20AC]);
(%o2)                            [$, @pounds{}, @euro{}]
@end example

Warning: In wxMaxima with SBCL on Windows it is not possible to convert 
code points larger than 16 bit to characters when the external format 
has not been set to UTF-8. See @ref{adjust_external_format} for more information.

@c Command @U not supported by texinfo 5.
@c @example
@c (%i3) unicode(1D538);
@c (%o3)                                  @U{1d538}
@c @end example

CMUCL doesn't process code points larger than 16 bit. 
In these cases @code{unicode} returns @code{false}. 
@c Converting characters to UTF-8 octets and finally to Unicode serves as a workaround.
Converting a code point to a character via UTF-8 octets may serve as a workaround: 

@code{octets_to_string(unicode_to_utf8(code_point));}

@c Command @U not supported by texinfo 5.
@c @example
@c (%i4) octets_to_string(unicode_to_utf8(1D538));
@c (%o4)                                  @U{1d538}
@c @end example

See @ref{octets_to_string}, @ref{unicode_to_utf8}.

In case the underlying Lisp provides full Unicode support the character might be 
specified by its name. The following is possible in ECL, CLISP and SBCL, 
where in SBCL on Windows the external format has to be set to UTF-8.
@code{unicode(name)} is supported by CMUCL too but again limited to 16 bit 
characters. 

The string argument to @code{unicode} is basically the same string returned by 
@code{printf} using the "~@@c" specifier. 
But as shown below the prefix "#\" must be omitted. 
Underlines might be replaced by spaces and uppercase letters by lowercase ones.

Example (continued): Characters defined by names 
(Maxima with SBCL on GNU/Linux). 

@example
(%i3) printf(false, "~@@c", unicode(0DF));
(%o3)                    #\LATIN_SMALL_LETTER_SHARP_S
(%i4) unicode("LATIN_SMALL_LETTER_SHARP_S");
(%o4)                                  @ss{}
(%i5) unicode("Latin small letter sharp s");
(%o5)                                  @ss{}
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{unicode_to_utf8}
@deffn {Function} unicode_to_utf8 (@var{code_point}) 

Returns a list containing the UTF-8 code corresponding to the Unicode @var{code_point}.

Examples: Converting Unicode code points to UTF-8 and vice versa. 

@example
(%i1) ibase: obase: 16.$
(%i2) map(cint, ["$","@pounds{}","@euro{}"]);
(%o2)                           [24, 0A3, 20AC]
(%i3) map(unicode_to_utf8, %);
(%o3)                 [[24], [0C2, 0A3], [0E2, 82, 0AC]]
(%i4) map(utf8_to_unicode, %);
(%o4)                           [24, 0A3, 20AC]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{uppercasep}
@deffn {Function} uppercasep (@var{char})  
  
Returns @code{true} if @var{char} is an uppercase character. 

Note: See remarks on @ref{alphacharp}.

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{us_ascii_only}
@defvr {Variable} us_ascii_only

This option variable affects Maxima when the character encoding 
provided by the application which runs Maxima is UTF-8 but the 
external format of the Lisp reader is not equal to UTF-8. 

On GNU/Linux this is true when Maxima is built with GCL 
and on Windows in wxMaxima with GCL- and SBCL-builds. 
With SBCL it is recommended to change the external format to UTF-8. 
Setting @code{us_ascii_only} is unnecessary then. 
See @ref{adjust_external_format} for details. 

@code{us_ascii_only} is @code{false} by default. 
Maxima itself then (i.e. in the above described situation) parses the UTF-8 encoding.

When @code{us_ascii_only} is set to @code{true} it is assumed that all strings 
used as arguments to string processing functions do not contain Non-US-ASCII characters. 
Given that promise, Maxima avoids parsing UTF-8 and strings can be processed more efficiently.

@opencatbox
@category{Global variables}
@category{Package stringproc}
@closecatbox

@end defvr

@c -----------------------------------------------------------------------------
@anchor{utf8_to_unicode}
@deffn {Function} utf8_to_unicode (@var{list}) 

Returns a Unicode code point corresponding to the @var{list} which must contain 
the UTF-8 encoding of a single character.

Examples: See @ref{unicode_to_utf8}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@c -----------------------------------------------------------------------------
@node String Processing, Octets and Utilities for Cryptography, Characters, stringproc-pkg
@section String Processing

Position indices in strings are 1-indexed like in Maxima lists. 
See example in @ref{charat}.

@c -----------------------------------------------------------------------------
@anchor{charat}
@deffn {Function} charat (@var{string}, @var{n}) 

Returns the @var{n}-th character of @var{string}.
The first character in @var{string} is returned with @var{n} = 1. 

@example
(%i1) charat("Lisp",1);
(%o1)                           L
(%i2) charlist("Lisp")[1];
(%o2)                           L
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{charlist}
@deffn {Function} charlist (@var{string}) 

Returns the list of all characters in @var{string}. 

@example
(%i1) charlist("Lisp");
(%o1)                     [L, i, s, p]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{eval_string}
@deffn {Function} eval_string (@var{str})

Parse the string @var{str} as a Maxima expression and evaluate it.
The string @var{str} may or may not have a terminator (dollar sign @code{$} or semicolon @code{;}).
Only the first expression is parsed and evaluated, if there is more than one.

Complain if @var{str} is not a string.

Examples:

@example
(%i1) eval_string ("foo: 42; bar: foo^2 + baz");
(%o1)                       42
(%i2) eval_string ("(foo: 42, bar: foo^2 + baz)");
(%o2)                   baz + 1764
@end example

See also @ref{parse_string}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{parse_string}
@deffn {Function} parse_string (@var{str})

Parse the string @var{str} as a Maxima expression (do not evaluate it).
The string @var{str} may or may not have a terminator (dollar sign @code{$} or semicolon @code{;}).
Only the first expression is parsed, if there is more than one.

Complain if @var{str} is not a string.

Examples:

@example
(%i1) parse_string ("foo: 42; bar: foo^2 + baz");
(%o1)                    foo : 42
(%i2) parse_string ("(foo: 42, bar: foo^2 + baz)");
                                   2
(%o2)          (foo : 42, bar : foo  + baz)
@end example

See also @ref{eval_string}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{scopy}
@deffn {Function} scopy (@var{string}) 

Returns a copy of @var{string} as a new string. 

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sdowncase}
@deffn {Function} sdowncase @
@fname{sdowncase} (@var{string}) @
@fname{sdowncase} (@var{string}, @var{start})  @
@fname{sdowncase} (@var{string}, @var{start}, @var{end})

Like @ref{supcase} but uppercase characters are converted to lowercase. 

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sequal}
@deffn {Function} sequal (@var{string_1}, @var{string_2}) 

Returns @code{true} if @var{string_1} and @var{string_2} contain the same 
sequence of characters. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sequalignore}
@deffn {Function} sequalignore (@var{string_1}, @var{string_2})

Like @code{sequal} but ignores case which is only possible for non-US-ASCII 
characters when the underlying Lisp is able to recognize a character as an 
alphabetic character. See remarks on @ref{alphacharp}. 

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sexplode}
@deffn {Function} sexplode (@var{string})

@code{sexplode} is an alias for function @code{charlist}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{simplode}
@deffn {Function} simplode @
@fname{simplode} (@var{list})   @
@fname{simplode} (@var{list}, @var{delim})  

@code{simplode} takes a list of expressions and concatenates them into a string.
If no delimiter @var{delim} is specified, @code{simplode} uses no delimiter.
@var{delim} can be any string.

See also @mrefcomma{concat} @mrefcomma{sconcat} @mref{string} and @mrefdot{printf}

Examples:

@example
(%i1) simplode(["xx[",3,"]:",expand((x+y)^3)]);
(%o1)             xx[3]:y^3+3*x*y^2+3*x^2*y+x^3
(%i2) simplode( sexplode("stars")," * " );
(%o2)                   s * t * a * r * s
(%i3) simplode( ["One","more","coffee."]," " );
(%o3)                   One more coffee.
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sinsert}
@deffn {Function} sinsert (@var{seq}, @var{string}, @var{pos})  
Returns a string that is a concatenation of @code{substring(@var{string}, 1, @var{pos}-1)},
the string @var{seq} and @code{substring (@var{string}, @var{pos})}.
Note that the first character in @var{string} is in position 1.

Examples:

@example
(%i1) s: "A submarine."$
(%i2) concat( substring(s,1,3),"yellow ",substring(s,3) );
(%o2)                  A yellow submarine.
(%i3) sinsert("hollow ",s,3);
(%o3)                  A hollow submarine.
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sinvertcase}
@deffn {Function} sinvertcase @
@fname{sinvertcase} (@var{string}) @
@fname{sinvertcase} (@var{string}, @var{start}) @
@fname{sinvertcase} (@var{string}, @var{start}, @var{end})

Returns @var{string} except that each character from position @var{start} to @var{end} is inverted.
If @var{end} is not given,
all characters from @var{start} to the end of @var{string} are replaced.

Examples:

@example
(%i1) sinvertcase("sInvertCase");
(%o1)                      SiNVERTcASE
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{slength}
@deffn {Function} slength (@var{string}) 

Returns the number of characters in @var{string}. 

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{smake}
@deffn {Function} smake (@var{num}, @var{char}) 

Returns a new string with a number of @var{num} characters @var{char}. 

Example:

@example
(%i1) smake(3,"w");
(%o1)                          www
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{smismatch}
@deffn {Function} smismatch @
@fname{smismatch} (@var{string_1}, @var{string_2}) @
@fname{smismatch} (@var{string_1}, @var{string_2}, @var{test})

Returns the position of the first character of @var{string_1} at which @var{string_1} and @var{string_2} differ or @code{false}.
Default test function for matching is @code{sequal}.
If @code{smismatch} should ignore case, use @code{sequalignore} as test.

Example:

@example
(%i1) smismatch("seven","seventh");
(%o1)                           6
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{split}
@deffn {Function} split @
@fname{split} (@var{string}) @
@fname{split} (@var{string}, @var{delim}) @
@fname{split} (@var{string}, @var{delim}, @var{multiple})

Returns the list of all tokens in @var{string}.
Each token is an unparsed string.
@code{split} uses @var{delim} as delimiter.
If @var{delim} is not given, the space character is the default delimiter.
@var{multiple} is a boolean variable with @code{true} by default.
Multiple delimiters are read as one.
This is useful if tabs are saved as multiple space characters.
If @var{multiple} is set to @code{false}, each delimiter is noted.

Examples:

@example
(%i1) split("1.2   2.3   3.4   4.5");
(%o1)                 [1.2, 2.3, 3.4, 4.5]
(%i2) split("first;;third;fourth",";",false);
(%o2)               [first, , third, fourth]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sposition}
@deffn {Function} sposition (@var{char}, @var{string}) 
Returns the position of the first character in @var{string} which matches @var{char}.
The first character in @var{string} is in position 1.
For matching characters ignoring case see @ref{ssearch}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sremove}
@deffn {Function} sremove @
@fname{sremove} (@var{seq}, @var{string}) @
@fname{sremove} (@var{seq}, @var{string}, @var{test}) @
@fname{sremove} (@var{seq}, @var{string}, @var{test}, @var{start}) @
@fname{sremove} (@var{seq}, @var{string}, @var{test}, @var{start}, @var{end})

Returns a string like @var{string} but without all substrings matching @var{seq}.
Default test function for matching is @code{sequal}.
If @code{sremove} should ignore case while searching for @var{seq}, use @code{sequalignore} as test.
Use @var{start} and @var{end} to limit searching.
Note that the first character in @var{string} is in position 1.

Examples:

@example
(%i1) sremove("n't","I don't like coffee.");
(%o1)                   I do like coffee.
(%i2) sremove ("DO ",%,'sequalignore);
(%o2)                    I like coffee.
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sremovefirst}
@deffn {Function} sremovefirst @
@fname{sremovefirst} (@var{seq}, @var{string}) @
@fname{sremovefirst} (@var{seq}, @var{string}, @var{test}) @
@fname{sremovefirst} (@var{seq}, @var{string}, @var{test}, @var{start}) @
@fname{sremovefirst} (@var{seq}, @var{string}, @var{test}, @var{start}, @var{end})

Like @code{sremove} except that only the first substring that matches @var{seq} is removed.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sreverse}
@deffn {Function} sreverse (@var{string}) 

Returns a string with all the characters of @var{string} in reverse order. 

See also @mrefdot{reverse}

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{ssearch}
@deffn {Function} ssearch @
@fname{ssearch} (@var{seq}, @var{string}) @
@fname{ssearch} (@var{seq}, @var{string}, @var{test}) @
@fname{ssearch} (@var{seq}, @var{string}, @var{test}, @var{start}) @
@fname{ssearch} (@var{seq}, @var{string}, @var{test}, @var{start}, @var{end})

Returns the position of the first substring of @var{string} that matches the string @var{seq}.
Default test function for matching is @code{sequal}.
If @code{ssearch} should ignore case, use @code{sequalignore} as test.
Use @var{start} and @var{end} to limit searching.
Note that the first character in @var{string} is in position 1.

Example:

@example
(%i1) ssearch("~s","~@{~S ~@}~%",'sequalignore);
(%o1)                                  4
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{ssort}
@deffn {Function} ssort @
@fname{ssort} (@var{string}) @
@fname{ssort} (@var{string}, @var{test})

Returns a string that contains all characters from @var{string} in an order such there are no two successive characters @var{c} and @var{d} such that @code{test (@var{c}, @var{d})} is @code{false} and @code{test (@var{d}, @var{c})} is @code{true}.
Default test function for sorting is @var{clessp}.
The set of test functions is @code{@{clessp, clesspignore, cgreaterp, cgreaterpignore, cequal, cequalignore@}}.

Examples:

@example
(%i1) ssort("I don't like Mondays.");
(%o1)                    '.IMaddeiklnnoosty
(%i2) ssort("I don't like Mondays.",'cgreaterpignore);
(%o2)                 ytsoonnMlkIiedda.'   
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{ssubst}
@deffn {Function} ssubst @
@fname{ssubst} (@var{new}, @var{old}, @var{string}) @
@fname{ssubst} (@var{new}, @var{old}, @var{string}, @var{test}) @
@fname{ssubst} (@var{new}, @var{old}, @var{string}, @var{test}, @var{start}) @
@fname{ssubst} (@var{new}, @var{old}, @var{string}, @var{test}, @var{start}, @var{end})

Returns a string like @var{string} except that all substrings matching @var{old} are replaced by @var{new}.
@var{old} and @var{new} need not to be of the same length.
Default test function for matching is @code{sequal}.
If @code{ssubst} should ignore case while searching for old, use @code{sequalignore} as test.
Use @var{start} and @var{end} to limit searching.
Note that the first character in @var{string} is in position 1.

Examples:

@example
(%i1) ssubst("like","hate","I hate Thai food. I hate green tea.");
(%o1)          I like Thai food. I like green tea.
(%i2) ssubst("Indian","thai",%,'sequalignore,8,12);
(%o2)         I like Indian food. I like green tea.
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{ssubstfirst}
@deffn {Function} ssubstfirst @
@fname{ssubstfirst} (@var{new}, @var{old}, @var{string}) @
@fname{ssubstfirst} (@var{new}, @var{old}, @var{string}, @var{test}) @
@fname{ssubstfirst} (@var{new}, @var{old}, @var{string}, @var{test}, @var{start}) @
@fname{ssubstfirst} (@var{new}, @var{old}, @var{string}, @var{test}, @var{start}, @var{end})

Like @code{subst} except that only the first substring that matches @var{old} is replaced.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{strim}
@deffn {Function} strim (@var{seq},@var{string}) 

Returns a string like @var{string},
but with all characters that appear in @var{seq} removed from both ends. 

Examples:

@example
(%i1) "/* comment */"$
(%i2) strim(" /*",%);
(%o2)                        comment
(%i3) slength(%);
(%o3)                           7
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{striml}
@deffn {Function} striml (@var{seq}, @var{string}) 

Like @code{strim} except that only the left end of @var{string} is trimmed. 

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{strimr}
@deffn {Function} strimr (@var{seq}, @var{string}) 

Like @code{strim} except that only the right end of @var{string} is trimmed.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{stringp}
@deffn {Function} stringp (@var{obj}) 

Returns @code{true} if @var{obj} is a string.
See introduction for example.

@opencatbox
@category{Predicate functions}
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{substring}
@deffn {Function} substring @
@fname{substring} (@var{string}, @var{start}) @
@fname{substring} (@var{string}, @var{start}, @var{end})

Returns the substring of @var{string} beginning at position @var{start} and ending at position @var{end}.
The character at position @var{end} is not included.
If @var{end} is not given, the substring contains the rest of the string.
Note that the first character in @var{string} is in position 1.

Examples:

@example
(%i1) substring("substring",4);
(%o1)                        string
(%i2) substring(%,4,6);
(%o2)                          in
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{supcase}
@deffn {Function} supcase @
@fname{supcase} (@var{string}) @
@fname{supcase} (@var{string}, @var{start}) @
@fname{supcase} (@var{string}, @var{start}, @var{end})

Returns @var{string} except that lowercase characters from position @var{start} to @var{end} are replaced by the corresponding uppercase ones.
If @var{end} is not given,
all lowercase characters from @var{start} to the end of @var{string} are replaced.

Example:

@example
(%i1) supcase("english",1,2);
(%o1)                        English
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{tokens}
@deffn {Function} tokens @
@fname{tokens} (@var{string}) @
@fname{tokens} (@var{string}, @var{test})

Returns a list of tokens, which have been extracted from @var{string}.
The tokens are substrings whose characters satisfy a certain test function.
If test is not given, @var{constituent} is used as the default test.
@code{@{constituent, alphacharp, digitcharp, lowercasep, uppercasep, charp, characterp, alphanumericp@}} is the set of test functions. 
(The Lisp-version of @code{tokens} is written by Paul Graham. ANSI Common Lisp, 1996, page 67.)

Examples:

@example
(%i1) tokens("24 October 2005");
(%o1)                  [24, October, 2005]
(%i2) tokens("05-10-24",'digitcharp);
(%o2)                     [05, 10, 24]
(%i3) map(parse_string,%);
(%o3)                      [5, 10, 24]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@c -----------------------------------------------------------------------------
@node Octets and Utilities for Cryptography,  , String Processing, stringproc-pkg
@section Octets and Utilities for Cryptography

@c -----------------------------------------------------------------------------
@anchor{base64}
@deffn {Function} base64 (@var{arg})

Returns the base64-representation of @var{arg} as a string. 
The argument @var{arg} may be a string, a non-negative integer or a list of octets.

Examples:

@example
(%i1) base64: base64("foo bar baz");
(%o1)                          Zm9vIGJhciBiYXo=
(%i2) string: base64_decode(base64);
(%o2)                            foo bar baz
(%i3) obase: 16.$
(%i4) integer: base64_decode(base64, 'number);
(%o4)                       666f6f206261722062617a
(%i5) octets: base64_decode(base64, 'list);
(%o5)            [66, 6F, 6F, 20, 62, 61, 72, 20, 62, 61, 7A]
(%i6) ibase: 16.$
(%i7) base64(octets);
(%o7)                          Zm9vIGJhciBiYXo=
@end example

Note that if @var{arg} contains umlauts (resp. octets larger than 127) 
the resulting base64-string is platform dependend. 
However the decoded string will be equal to the original.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{base64_decode}
@deffn {Function} base64_decode @
@fname{base64_decode} (@var{base64-string}) @
@fname{base64_decode} (@var{base64-string}, @var{return-type})

By default @code{base64_decode} decodes the @var{base64-string} back to the original string. 

The optional argument @var{return-type} allows @code{base64_decode} to 
alternatively return the corresponding number or list of octets.
@var{return-type} may be @code{string}, @code{number} or @code{list}.

Example: See @ref{base64}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{crc24sum}
@deffn {Function} crc24sum @
@fname{crc24sum} (@var{octets}) @
@fname{crc24sum} (@var{octets}, @var{return-type})

By default @code{crc24sum} returns the @code{CRC24} checksum of an octet-list 
as a string.

The optional argument @var{return-type} allows @code{crc24sum} to 
alternatively return the corresponding number or list of octets.
@var{return-type} may be @code{string}, @code{number} or @code{list}.

Example:

@example
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v2.0.22 (GNU/Linux)

iQEcBAEBAgAGBQJVdCTzAAoJEG/1Mgf2DWAqCSYH/AhVFwhu1D89C3/QFcgVvZTM
wnOYzBUURJAL/cT+IngkLEpp3hEbREcugWp+Tm6aw3R4CdJ7G3FLxExBH/5KnDHi
rBQu+I7+3ySK2hpryQ6Wx5J9uZSa4YmfsNteR8up0zGkaulJeWkS4pjiRM+auWVe
vajlKZCIK52P080DG7Q2dpshh4fgTeNwqCuCiBhQ73t8g1IaLdhDN6EzJVjGIzam
/spqT/sTo6sw8yDOJjvU+Qvn6/mSMjC/YxjhRMaQt9EMrR1AZ4ukBF5uG1S7mXOH
WdiwkSPZ3gnIBhM9SuC076gLWZUNs6NqTeE3UzMjDAFhH3jYk1T7mysCvdtIkms=
=WmeC
-----END PGP SIGNATURE-----
@end example

@example
(%i1) ibase : obase : 16.$
(%i2) sig64 : sconcat(
 "iQEcBAEBAgAGBQJVdCTzAAoJEG/1Mgf2DWAqCSYH/AhVFwhu1D89C3/QFcgVvZTM",
 "wnOYzBUURJAL/cT+IngkLEpp3hEbREcugWp+Tm6aw3R4CdJ7G3FLxExBH/5KnDHi",
 "rBQu+I7+3ySK2hpryQ6Wx5J9uZSa4YmfsNteR8up0zGkaulJeWkS4pjiRM+auWVe",
 "vajlKZCIK52P080DG7Q2dpshh4fgTeNwqCuCiBhQ73t8g1IaLdhDN6EzJVjGIzam",
 "/spqT/sTo6sw8yDOJjvU+Qvn6/mSMjC/YxjhRMaQt9EMrR1AZ4ukBF5uG1S7mXOH",
 "WdiwkSPZ3gnIBhM9SuC076gLWZUNs6NqTeE3UzMjDAFhH3jYk1T7mysCvdtIkms=" )$
(%i3) octets: base64_decode(sig64, 'list)$
(%i4) crc24: crc24sum(octets, 'list);
(%o4)                          [5A, 67, 82]
(%i5) base64(crc24);
(%o5)                              WmeC
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{md5sum}
@deffn {Function} md5sum @
@fname{md5sum} (@var{arg}) @
@fname{md5sum} (@var{arg}, @var{return-type})

Returns the @code{MD5} checksum of a string, a non-negative integer or 
a list of octets. The default return value is a string containing 32 hex characters.

The optional argument @var{return-type} allows @code{md5sum} to alternatively 
return the corresponding number or list of octets.
@var{return-type} may be @code{string}, @code{number} or @code{list}.

Examples:

@example
(%i1) ibase: obase: 16.$
(%i2) msg: "foo bar baz"$
(%i3) string: md5sum(msg);
(%o3)                  ab07acbb1e496801937adfa772424bf7
(%i4) integer: md5sum(msg, 'number);
(%o4)                 0ab07acbb1e496801937adfa772424bf7
(%i5) octets: md5sum(msg, 'list);
(%o5)        [0AB,7,0AC,0BB,1E,49,68,1,93,7A,0DF,0A7,72,42,4B,0F7]
(%i6) sdowncase( printf(false, "~@{~2,'0x~^:~@}", octets) );
(%o6)           ab:07:ac:bb:1e:49:68:01:93:7a:df:a7:72:42:4b:f7
@end example

Note that in case @var{arg} contains German umlauts or other non-ASCII 
characters (resp. octets larger than 127) the @code{MD5} checksum is platform dependend.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{mgf1_sha1}
@deffn {Function} mgf1_sha1 @
@fname{mgf1_sha1} (@var{seed}, @var{len}) @
@fname{mgf1_sha1} (@var{seed}, @var{len}, @var{return-type})

Returns a pseudo random number of variable length. 
By default the returned value is a number with a length of @var{len} octets.

The optional argument @var{return-type} allows @code{mgf1_sha1} to alternatively 
return the corresponding list of @var{len} octets.
@var{return-type} may be @code{number} or @code{list}.

The computation of the returned value is described in @code{RFC 3447}, 
appendix @code{B.2.1 MGF1}. 
@code{SHA1} ist used as hash function, i.e. the randomness of the computed number 
relies on the randomness of @code{SHA1} hashes.

Example:

@example
(%i1) ibase: obase: 16.$
(%i2) number: mgf1_sha1(4711., 8);
(%o2)                        0e0252e5a2a42fea1
(%i3) octets: mgf1_sha1(4711., 8, 'list);
(%o3)                  [0E0,25,2E,5A,2A,42,0FE,0A1]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{number_to_octets}
@deffn {Function} number_to_octets (@var{number})

Returns an octet-representation of @var{number} as a list of octets.
The @var{number} must be a non-negative integer.

Example:

@example
(%i1) ibase : obase : 16.$
(%i2) octets: [0ca,0fe,0ba,0be]$
(%i3) number: octets_to_number(octets);
(%o3)                            0cafebabe
(%i4) number_to_octets(number);
(%o4)                      [0CA, 0FE, 0BA, 0BE]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{octets_to_number}
@deffn {Function} octets_to_number (@var{octets})

Returns a number by concatenating the octets in the list of @var{octets}.

Example: See @ref{number_to_octets}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{octets_to_oid}
@deffn {Function} octets_to_oid (@var{octets})

Computes an object identifier (OID) from the list of @var{octets}.

Example: RSA encryption OID

@example
(%i1) ibase : obase : 16.$
(%i2) oid: octets_to_oid([2A,86,48,86,0F7,0D,1,1,1]);
(%o2)                      1.2.840.113549.1.1.1
(%i3) oid_to_octets(oid);
(%o3)               [2A, 86, 48, 86, 0F7, 0D, 1, 1, 1]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{octets_to_string}
@deffn {Function} octets_to_string @
@fname{octets_to_string} (@var{octets})  @
@fname{octets_to_string} (@var{octets}, @var{encoding}) 

Decodes the list of @var{octets} into a string according to current system defaults. 
When decoding octets corresponding to Non-US-ASCII characters 
the result depends on the platform, application and underlying Lisp.

Example: Using system defaults 
(Maxima compiled with GCL, which uses no format definition and 
simply passes through the UTF-8-octets encoded by the GNU/Linux terminal).

@example
(%i1) octets: string_to_octets("abc");
(%o1)                            [61, 62, 63]
(%i2) octets_to_string(octets);
(%o2)                                 abc
(%i3) ibase: obase: 16.$
(%i4) unicode(20AC);
(%o4)                                  @euro{}
(%i5) octets: string_to_octets(%);
(%o5)                           [0E2, 82, 0AC]
(%i6) octets_to_string(octets);
(%o6)                                  @euro{}
(%i7) utf8_to_unicode(octets);
(%o7)                                20AC
@end example

In case the external format of the Lisp reader is equal to UTF-8 the optional 
argument @var{encoding} allows to set the encoding for the octet to string conversion. 
If necessary see @ref{adjust_external_format} for changing the external format. 

Some names of supported encodings (see corresponding Lisp manual for more): @*
CCL, CLISP, SBCL: @code{utf-8, ucs-2be, ucs-4be, iso-8859-1, cp1252, cp850} @*
CMUCL: @code{utf-8, utf-16-be, utf-32-be, iso8859-1, cp1252} @*
ECL: @code{utf-8, ucs-2be, ucs-4be, iso-8859-1, windows-cp1252, dos-cp850} 
   
Example (continued): Using the optional encoding argument 
(Maxima compiled with SBCL, GNU/Linux terminal).

@example
(%i8) string_to_octets("@euro{}", "ucs-2be");
(%o8)                              [20, 0AC]
@end example

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{oid_to_octets}
@deffn {Function} oid_to_octets (@var{oid-string})

Convertes an object identifier (OID) to a list of @var{octets}.

Example: See @ref{octets_to_oid}.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sha1sum}
@deffn {Function} sha1sum @
@fname{sha1sum} (@var{arg}) @
@fname{sha1sum} (@var{arg}, @var{return-type})

Returns the @code{SHA1} fingerprint of a string, a non-negative integer or 
a list of octets. The default return value is a string containing 40 hex characters.

The optional argument @var{return-type} allows @code{sha1sum} to alternatively 
return the corresponding number or list of octets.
@var{return-type} may be @code{string}, @code{number} or @code{list}.

Example:

@example
(%i1) ibase: obase: 16.$
(%i2) msg: "foo bar baz"$
(%i3) string: sha1sum(msg);
(%o3)              c7567e8b39e2428e38bf9c9226ac68de4c67dc39
(%i4) integer: sha1sum(msg, 'number);
(%o4)             0c7567e8b39e2428e38bf9c9226ac68de4c67dc39
(%i5) octets: sha1sum(msg, 'list);
(%o5)  [0C7,56,7E,8B,39,0E2,42,8E,38,0BF,9C,92,26,0AC,68,0DE,4C,67,0DC,39]
(%i6) sdowncase( printf(false, "~@{~2,'0x~^:~@}", octets) );
(%o6)     c7:56:7e:8b:39:e2:42:8e:38:bf:9c:92:26:ac:68:de:4c:67:dc:39
@end example

Note that in case @var{arg} contains German umlauts or other non-ASCII 
characters (resp. octets larger than 127) the @code{SHA1} fingerprint is platform dependend.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{sha256sum}
@deffn {Function} sha256sum @
@fname{sha256sum} (@var{arg}) @
@fname{sha256sum} (@var{arg}, @var{return-type})

Returns the @code{SHA256} fingerprint of a string, a non-negative integer or 
a list of octets. The default return value is a string containing 64 hex characters.

The optional argument @var{return-type} allows @code{sha256sum} to alternatively 
return the corresponding number or list of octets (see @ref{sha1sum}).

Example:

@example
(%i1) string: sha256sum("foo bar baz");
(%o1)  dbd318c1c462aee872f41109a4dfd3048871a03dedd0fe0e757ced57dad6f2d7
@end example

Note that in case @var{arg} contains German umlauts or other non-ASCII 
characters (resp. octets larger than 127) the @code{SHA256} fingerprint is platform dependend.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{string_to_octets}
@deffn {Function} string_to_octets @
@fname{string_to_octets} (@var{string})  @
@fname{string_to_octets} (@var{string}, @var{encoding}) 

Encodes a @var{string} into a list of octets according to current system defaults. 
When encoding strings containing Non-US-ASCII characters 
the result depends on the platform, application and underlying Lisp.

In case the external format of the Lisp reader is equal to UTF-8 the optional 
argument @var{encoding} allows to set the encoding for the string to octet conversion. 
If necessary see @ref{adjust_external_format} for changing the external format. 

See @ref{octets_to_string} for examples and some more information.

@opencatbox
@category{Package stringproc}
@closecatbox

@end deffn
