---
title: SLOANE(1) User Manual | Version 4.0.2
date: 30 Aug 2015
---

# NAME

sloane: A command line interface to Sloane's OEIS.

# SYNOPSIS

**sloane** `OPTIONS --oeis TERMS...`  
**sloane** `OPTIONS (-q|--query) A-NUMBER/SEQUENCE...`  
**sloane** `[--invert] --filter`  

# DESCRIPTION

The `sloane` command provides an interface to OEIS (The On-Line
Encyclopedia of Integer Sequences). It can be used online with the
`--oeis` operation, and offline with the `--query` operation; offline
mode is the default.

# OPERATION --oeis

## Synopsis

`sloane [--long] [-k KEYS] [-n N] [--all] [--monochrome] [--json] --oeis TERMS...`

## Description

Lookup search terms in Sloane's On-Line Encyclopedia of Integer
Sequences (OEIS).

## Options

--long
:   Long format; print all fields.

-k *KEYS*
:   Keys of fields to print (default: SN).

-n *N*
:   Fetch at most this many entries (default: 5).

--all
:   Fetch all matching entries (equivalent to -n 999999).

--monochrome
:   Do not colorize the output. Useful when piping the output to another
    program.

--json
:   Output results is JSON format.

## Examples

The most common search is for entries matching a sequence of consecutive terms:

    sloane --oeis 1,3,19,183,2371,38703

At the time of writing this particular query would return

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.

As this illustrates, the default is to return just the sequence (S) and
the name (N) fields. To override the default one can use the keys
option. For instance, the following search shows the sequence, name,
comments, and formula fields of the sequence whose A-number is A006531:

    sloane -k SNCF --oeis id:A006531

The next example returns at most 3 results of a free text search:

    sloane -n 3 --oeis "(2+2)-free posets"

Sloane normally crops long lines to fit the widths of the terminal. If
this is unwanted, pipe the output through cat or less:

    sloane --long --oeis id:A000110 | less -R

# OPERATION --query

## Synopsis

`sloane [--long] [-k KEYS] [-n N] [--all] [--monochrome] [--json] (-q|--query) A-NUMBER/SEQUENCE...`

## Description

Search locally against a downloaded local database of known
sequences. This type of query is less flexible, but faster, than using
the `--oeis` operation: With `--query` one can only lookup
A-numbers and seqences; free text searches are not supported. Also, the
fields returned by `--query` are 'S' and 'N'.

## Examples

Lookup A-numbers:

    $ sloane --query A000111 A000112
    
    S A000111 1,1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,
    N A000111 Euler or up/down numbers: e.g.f. sec(x) + tan(x)..
    
    S A000112 1,1,2,5,16,63,318,2045,16999,183231,2567284,46749427,
    N A000112 Number of partially ordered sets ("posets") with n..

Lookup a sequence:

    $ sloane -n2 -q 1,1,2,5,15,52,203,877,4140,21147,115975,678570,
    
    S A000110 1,1,2,5,15,52,203,877,4140,21147,115975,678570,4213597,
    N A000110 Bell or exponential numbers: number of ways to partition..
    
    S A192128 1,1,2,5,15,52,203,877,4140,21147,115975,678570,4213597,
    N A192128 Number of set partitions of {1, ..., n} that avoid..

## Options

Same as for the `--oeis` operation.

# OPERATION --filter

## Synopsis

`sloane [--invert] --filter`

## Description

To check a large number of sequences one can use `--filter`.  When this
option is set, `sloane` reads the standard input line-by-line, if the
sequence read is in the local database, then it is returned to the
standard output; if not, it is ignored. This way one can quickly filter
out the sequences from the input that are in the local database. In
other words, assuming that *FILE* contains one sequence per line,

    sloane --filter <FILE

returns the subset of the sequences in *FILE* that are in the local
database. To also lookup the names of those sequences one could, for
instance, run

    sloane --filter <FILE | sloane -q

## Flag

--invert
:   Return sequences *not* in the database.

# ADDITIONAL OPERATIONS

--update
:   Update the local database.

--version
:   Print version information.

--help
:   Briefly describe the available options.


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

Please use the `--oeis` option with moderation as not to overburden the
OEIS-server; see OEIS' policy on searching the database:
<http://oeis.org/wiki/Welcome#Policy_on_Searching_the_Database>.

# SEE ALSO

<http://akc.is/src/sloane>

# AUTHOR

Anders Claesson <http://akc.is>
