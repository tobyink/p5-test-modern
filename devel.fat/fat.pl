#!/usr/bin/env perl

use App::FatPacker ();
use Pod::Stripper ();
use PIR ();

my $tracefile = 'fatpacker.trace';
my $infile    = 'lib/Test/Modern.pm';
my $outfile   = 'devel.fat/lib/Test/Modern.pm';

-e($_) && die("$_ already exists!")
	for 'fatlib', $tracefile, 'packlists';

# Trace deps
system fatpack => trace => $infile;

# Decide which things we really want to keep
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
	
	superclass
	version
	Devel::Symdump
); # everything else should be in core, more or less

# Rewrite trace file
my @lines =
	sort
	grep m{^($keepers)\b},
	map { chomp; $_ }
	do { local(@ARGV) = $tracefile; <> };
open my $fh, '>', $tracefile;
print $fh "$_\n" for @lines;
close $fh;

# Assemble fatlib
system "fatpack packlists-for `cat $tracefile` >packlists";
system "fatpack tree `cat packlists`";

# Strip pod (this roughly halves the output module size)
for my $file ( PIR->new->file->all('fatlib') )
{
	system podstrip => $file, "tmp.pm";
	system mv => "tmp.pm", $file;
}

# Fatpack!
system "fatpack file $infile >$outfile";

# Clean up
system rm => '-rf' => 'fatlib', $tracefile, 'packlists';
