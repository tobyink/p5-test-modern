#!/usr/bin/env perl

my $tracefile = 'fatpacker.trace';
my $infile    = 'lib/Test/Modern.pm';
my $outfile   = 'devel.fat/lib/Test/Modern.pm';

system fatpack => trace => $infile;

my $keepers = join '|', map quotemeta(s{::}{/}r), qw(
	Exporter::Tiny
	Import::Into
	Module::Runtime
	Test::API
	Test::Deep
	Test::Fatal
	Test::LongString
	Test::Warnings
	Try::Tiny
);

my @lines =
	sort
	grep m{^($keepers)\b},
	map { chomp; $_ }
	do { local(@ARGV) = $tracefile; <> };

open my $fh, '>', $tracefile;
print $fh "$_\n" for @lines;
close $fh;

system "fatpack packlists-for `cat $tracefile` >packlists";
system "fatpack tree `cat packlists`";
system "fatpack file $infile >$outfile";
system rm => '-f', $tracefile, 'packlists';
system rm => qw( -rf fatlib );
