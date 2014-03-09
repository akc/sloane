---
title: SLOANE(1) Sloane User Manual | Version 1.3
date: March 9, 2014
---

# NAME

sloane - a command line interface to Sloane's
On-Line Encyclopedia of Integer Sequences (OEIS) <http://oeis.org>

# SYNOPSIS

sloane [*options*] *search-terms*

# DESCRIPTION

The sloane command searches Sloane's On-Line Encyclopedia of Integer
Sequences (OEIS). The search terms are typically the leading term of a
sequence, but can be a sequence id (its A-number) or even arbitrary words.

# OPTIONS

-k --keys=KEYS
:   Keys of fields to print (default: SN)

-a --all
:   Print all fields

-u --url
:   Only print urls of found entries

-n --limit=INT
:   Retrieve at most this many entries (default: 5)

-? --help
:   Display a short help message

-V --version
:   Print version information

# EXAMPLES

The most common search is for entries matching a sequence of consecutive terms:

~~~~~
sloane 1,3,19,183,2371,38703
~~~~~

At the time of writing this particular query would return

~~~~~
S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
N A006531 Semiorders on n elements.
~~~~~

As this illustrates, the default is to return just the sequence (S) and
the name (N) fields. To override the default one can use the keys
option. For instance, the following search shows the sequence, name,
comments, and formula fields of the sequence whose A-number is A006531:

~~~~~
sloane -kSNCF id:A006531
~~~~~

The next example returns at most 3 results of a free text search:

~~~~~
sloane -n3 "(2+2)-free posets"
~~~~~

To view the full entries of these 3 results in a browser (e.g., firefox)
one can use the url option:

~~~~~
firefox `sloane --url -n3 "(2+2)-free posets"`
~~~~~

# KEYS

These are the keys used by OEIS <http://oeis.org/eishelp2.html>.

    I  ID number

    S  1st line of unsigned sequence
    T  2nd line of unsigned sequence
    U  3rd line of unsigned sequence

    V  1st line of signed sequence
    W  2nd line of signed sequence
    X  3rd line of signed sequence

    N  Name
    C  Comments
    D  References
    H  Links
    F  Formula
    e  Examples

    p  Maple program
    t  Mathematica program
    o  Program in other language

    Y  Cross-references
    K  Keywords
    O  Offset
    A  Author
    E  Extensions and errors

# NOTES

Please use this program with moderation as not to overburden the
OEIS-server; see OEIS' policy on searching the database:
<http://oeis.org/wiki/Welcome#Policy_on_Searching_the_Database>.

# SEE ALSO

The sloane source code may be downloaded from
<https://github.com/akc/sloane>.

# AUTHOR

Anders Claesson
