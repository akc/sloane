---
title: SLOANE(1) Sloane User Manual | Version 1.9
date: July 21, 2014
---

# NAME

sloane - a command line interface to Sloane's
On-Line Encyclopedia of Integer Sequences <http://oeis.org>

# SYNOPSIS

sloane (lookup | grep)
  [-a | --all | -k *keys* | --url] [-n *entries*] *search-terms* ...  
sloane filter [--invert]  
sloane update  
sloane version  

# DESCRIPTION

The `sloane lookup` command searches Sloane's On-Line Encyclopedia of
Integer Sequences (OEIS). The search terms are typically the leading
term of a sequence. For example,

    sloane lookup 1,1,2,5,15,52,203,877,4140

returns entry A000110 (Bell numbers), and four more entries. One can
also search by sequence id (A-number), or even search for arbitrary
words. See the **EXAMPLES** section. Alternatively, using the `sloane
grep` command, the search can be done locally against a downloaded list
of known sequences. This mode works by "grepping" for the query in the
sequence field.

To check a large number of sequences one can use the `sloane filter`
command.  It reads the standard input line-by-line, if the sequence read
is in the local database, then it is returned to the standard output; if
not, it is ignored. This way onw can quickly filter out the sequences
from the input that are in the local database. In other words, assuming
that *FILE* contains one sequence per line,

    sloane filter <FILE

returns the subset of the sequences in *FILE* that are in the local
database. To also look-up the names of those sequences one could, for
instance, run

    sloane filter <FILE | xargs -L1 --verbose sloane grep

Sloane normally crops long lines to fit the widths of the terminal. If
this is unwanted, pipe the output through cat or less:

    sloane lookup -a id:A000110 | less -R

# OPTIONS

--help
:   Display a short help message

# COMMANDS

## `lookup`

Lookup a sequence, or other search term, in OEIS

-a, --all
:   Print all fields

-k *keys*
:   Keys of fields to print (default: SN)

--url
:   Print URLs of found entries (but nothing else)

-n *entries*
:   Fetch at most this many entries (default: 5)


## `grep`

Grep for a sequence in the local database. Same options as for the
`lookup` command apply.

## `filter`

Read sequences from stdin and return those that are in the local
database.

--invert
:   Return sequences *not* in the database.

## `update`

Update the local database.

## `version`

Print version information.

# EXAMPLES

The most common search is for entries matching a sequence of consecutive terms:

    sloane lookup 1,3,19,183,2371,38703

At the time of writing this particular query would return

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.

As this illustrates, the default is to return just the sequence (S) and
the name (N) fields. To override the default one can use the keys
option. For instance, the following search shows the sequence, name,
comments, and formula fields of the sequence whose A-number is A006531:

    sloane lookup -k SNCF id:A006531

The next example returns at most 3 results of a free text search:

    sloane lookup -n 3 "(2+2)-free posets"

To view the full entries of these 3 results in a browser (e.g., Firefox)
one can use the url option:

    firefox `sloane lookup --url -n 3 "(2+2)-free posets"`

In the final example the local cache is used to filter out sequences
from the standard input that are in OEIS:

    sloane filter <<END
    1,2,3,6,11,23,47,106,235           # Comma separated integers
    1 2 444 90 120                     # Space separated integers
    '(3 9 27 88 123)                   # S-expression
    [2,3,5,7,11,13,17,19,23,29,31,37]  # Haskell list
    [1; 1; 2; 3; 5; 8; 13; 21]         # O'Caml list
    {1, -1, 2, -6, 24, -120, 720}      # Mathematica list
    END

The '#' character and any text after it is ignored by sloane. Note also
that sloane is quite liberal in the formatting of sequences it accepts.

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

Anders Claesson <http://akc.is>
