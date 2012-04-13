#!/usr/bin/env perl

use warnings;
use strict;

# usage: find . -name '*.scala' | xargs scaladefs.pl >> JUMPER

while (<>) {
    /(?:class|object) ([a-zA-Z_]+)/ and print "$1\t$ARGV\t$.\n";

} continue {
    close ARGV if eof;
}

__END__
