#!/usr/bin/env perl

use warnings;
use strict;

# usage: find . -name '*.rb' | xargs rubydefs.pl > JUMPER

while (<>) {
    /def (?:self\.)?([a-zA-Z_?!]+)/ and print "$1\t$ARGV\t$.\n";
    /class (\w+)/ and print "$1\t$ARGV\t$.\n";
    /^\s*([A-Z_]+)\s*=/ and print "$1\t$ARGV\t$.\n";
} continue {
    close ARGV if eof;  # Not eof()!
}

__END__
