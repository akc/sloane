---
title: SLOANE(1) Sloane User Manual | Version 2.0.2
date: 7 Jan 2015
---

# NAME

sloane - a command line interface to Sloane's
On-Line Encyclopedia of Integer Sequences <http://oeis.org>

# SYNOPSIS

sloane [-a|--all] [-k KEYS] [-n N] [--url] [--local] TERMS...

sloane -A NUMBER

sloane --filter [--invert]

sloane --transform NAME

sloane (--list-transforms | --update | --version | --help)

# DESCRIPTION

The `sloane` command searches Sloane's On-Line Encyclopedia of Integer
Sequences (OEIS). The search terms are typically the leading term of a
sequence. For example,

    sloane 1,1,2,5,15,52,203,877,4140

returns entry A000110 (Bell numbers), and four more entries.  One can
also search by sequence id (A-number), or even search for arbitrary
words. See the **EXAMPLES** section.

Alternatively, using the `--local` option, the search can be done
locally against a downloaded local database of known sequences. This
mode works by "grepping" for the query in the sequence field.

To check a large number of sequences one can use `--filter`.  When this
option is set, `sloane` reads the standard input line-by-line, if the
sequence read is in the local database, then it is returned to the
standard output; if not, it is ignored. This way one can quickly filter
out the sequences from the input that are in the local database. In
other words, assuming that *FILE* contains one sequence per line,

    sloane --filter <FILE

returns the subset of the sequences in *FILE* that are in the local
database. To also look-up the names of those sequences one could, for
instance, run

    sloane --filter <FILE | xargs -L1 --verbose sloane --local

Sloane normally crops long lines to fit the widths of the terminal. If
this is unwanted, pipe the output through cat or less:

    sloane -a id:A000110 | less -R

# OPTIONS

-a, --all
:   Print all fields

-k *KEYS*
:   Keys of fields to print (default: SN)

-n *N*
:   Fetch at most this many entries (default: 5)

--url
:   Print URLs of found entries (but nothing else)

--local
:   Grep for a sequence in the local database.

-A *NUMBER*
:   Fetch the sequence with this number from the local database. Prints
    the sequence, but nothing else, to sdout.

--filter
:   Read sequences from stdin and return those that are in the local
    database.

--invert
:   Return sequences *not* in the database. This option has no effect
    unless `--filter` is also set.

--transform *NAME*
:   Apply the named transform to the input sequence. If the resulting
    sequence is integral print it to stdout; else print nothing.
    Most of the transforms and their names are taken from
    <https://oeis.org/transforms.txt>.

--list-transforms
:   List the names of all transforms.

--update
:   Update the local database.

--version
:   Print version information.

--help
:   Briefly describe the available options.

# EXAMPLES

The most common search is for entries matching a sequence of consecutive terms:

    sloane 1,3,19,183,2371,38703

At the time of writing this particular query would return

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.

As this illustrates, the default is to return just the sequence (S) and
the name (N) fields. To override the default one can use the keys
option. For instance, the following search shows the sequence, name,
comments, and formula fields of the sequence whose A-number is A006531:

    sloane -k SNCF id:A006531

The next example returns at most 3 results of a free text search:

    sloane -n 3 "(2+2)-free posets"

To view the full entries of these 3 results in a browser (e.g., Firefox)
one can use the url option:

    firefox `sloane --url -n 3 "(2+2)-free posets"`

To retrieve sequence A022493 from the local database use the `-A` option:

    sloane -A022493

In the final example the local database is used to filter out sequences
from the standard input that are in OEIS:

    sloane --filter <<END
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
