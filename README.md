sloane
======

A command line interface to Sloane's On-Line Encyclopedia of Integer Sequences (OEIS)

Install
-------

    $ cabal install sloane

Usage
-----

    $ sloane --help
    Search Sloane's On-Line Encyclopedia of Integer Sequences

    sloane [OPTIONS] SEARCH-TERMS

    Common flags:
      -k --keys=KEYS  Keys of fields to print, http://oeis.org/eishelp1.html
                      (default: SN)
      -a --all        Print all fields
      -n --limit=INT  Limit the number of entries retrieved (default: 5)
      -? --help       Display help message
      -V --version    Print version information

Examples
--------

Search for entries matching a sequence:

    $ sloane 1,3,19,183,2371,38703

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.

Show the sequence, name, comments, and formula fields of the sequence whose A-number is A006531:

    $ sloane -kSNCF id:A006531

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.
    C A006531 Labeled semiorders on n elements: (1+3) and (2+2)-free posets. - Detlef Pauly (dettodet(AT)yahoo.de), Dec 27 2002
    F A006531 O.g.f.: Sum_{n>=1} (2*n)!/(n+1)! * x^n / Product_{k=0..n} (1+k*x). [From Paul D. Hanna, Jul 20 2011]
    F A006531 E.g.f.: C(1-exp(-x)), where C(x) = (1 - sqrt(1 - 4*x)) / (2*x) is the ordinary g.f. for the Catalan numbers A000108.
    F A006531 a(n) = sum( S(n, k) * k! * M(k-1), k=1..n), S(n, k): Stirling number of the second kind, M(n): Motzkin number, A001006. - Detlef Pauly, Jun 06 2002

Return at most 3 results of a free text search:

    $ sloane -n3 "(2+2)-free posets"

    S A079144 1,1,3,19,207,3451,81663,2602699,107477247,5581680571,356046745023,
    N A079144 Number of labeled interval orders on n elements: (2+2)-free posets.

    S A022493 1,1,2,5,15,53,217,1014,5335,31240,201608,1422074,10886503,89903100,
    N A022493 Number of linearized chord diagrams of degree n; also number of nonisomorphic interval orders on n unlabeled points.

    S A006531 1,1,3,19,183,2371,38703,763099,17648823,468603091,14050842303,
    N A006531 Semiorders on n elements.

Caveat
------

Please use this program with moderation as not to put too much of a burden on the OEIS-server; see
[OEIS' policy on searching the database](http://oeis.org/wiki/Welcome#Policy_on_Searching_the_Database).
