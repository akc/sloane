#!/bin/sh

n=0

check () {
  n=$((n + 1))
  out=`eval $1`
  if [ "$out" = "$2" ]; then
      printf "Test #%d passed\n" $n
  else
      printf "Test #%d FAILED:\n\n" $n
      printf "COMMAND:\n%s\n\n" "$1"
      printf "EXPECTED OUTPUT:\n%s\n\n" "$2"
      printf "ACTUAL OUTPUT:\n%s\n\n" "$out"
      exit 1
  fi
}

check 'sloane --monochrome -q A001111' "`cat <<EOF

S A001111 1,1,1,5,6,1106,208310,10374196953
N A001111 Number of inequivalent Hadamard designs of order 4n.

EOF`"

check 'sloane --monochrome -q A000001 1,1,1,5,6,1106,208310,10374196953' \
"`cat <<EOF
query: A000001

S A000001 0,1,1,1,2,1,2,1,5,2,2,1,5,1,2,1,14,1,5,1,5,2,2,1,15,2,2,5,4,1,4,\
1,51,1,2,1,14,1,2,2,14,1,6,1,4,2,2,1,52,2,5,1,5,1,15,2,13,2,2,1,13,1,2,4,\
267,1,4,1,5,1,4,1,50,1,2,3,4,1,6,1,52,15,2,1,15,1,2,1,12,1,10,1,4,2
N A000001 Number of groups of order n.

query: 1,1,1,5,6,1106,208310,10374196953

S A001111 1,1,1,5,6,1106,208310,10374196953
N A001111 Number of inequivalent Hadamard designs of order 4n.

EOF`"
