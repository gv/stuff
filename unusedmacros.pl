#!/usr/bin/perl

# Print unused macros
# Copyright (C) 2013 Alexander Cherepanov <cherepan at mccme.ru>
#
# Slighlty modifed 2015 by vg (added reset on #include)
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted.

# Usage:
#   unused-macros *.c

use strict;
use warnings;

for my $file (@ARGV) {
    my %def;
    open IN, '<', $file;
    my $k;
    while (<IN>) {
        $k++;
        if (s/^#\s*define\s+(\w+)//) {
            $def{$1} = $k;
        }
        if (s/^#\s*include\s+//) {
            undef %def;
        }
        while (/\w+/g) {
            delete $def{$&};
        }
    }
    close IN;

    # Ignore some macros from https://gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
    delete @def{qw(_POSIX_SOURCE _BSD_SOURCE _XOPEN_SOURCE _XOPEN_SOURCE_EXTENDED _LARGEFILE64_SOURCE _GNU_SOURCE)};
    # ?
    #delete @def{qw(__POSIX _XOPEN_VERSION)};

    # To sort by line numbers we need to reverse out hash
    my %rev_def = reverse %def;
    for my $lineno (sort { $a <=> $b } keys %rev_def) { # numeric sort
        my $macro = $rev_def{$lineno};
        if (!($macro =~ /^NEED_OS_/)) { # ignore john specific macros NEED_OS_*
            print "$file:$def{$macro}: warning: unused macro '$macro'\n";
        }
    }
}

