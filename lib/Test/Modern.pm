use 5.008;
use strict;
use warnings;

package Test::Modern;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.000_05';

use Exporter::Tiny   0.030 qw();
use Import::Into     1.002 qw();
use Module::Runtime  0.012 qw( require_module );
use Test::More       0.96;
use Test::API        0.003;
use Test::Fatal      0.007;
use Test::Warnings   0.009 qw( warning warnings );
use Test::LongString 0.15;
use Test::Deep       0.111 qw( :v1 );
use Try::Tiny        0.15  qw( try catch );

my %HINTS;

$HINTS{ extended } = sub
{
	return if $ENV{EXTENDED_TESTING};
	plan skip_all => 'Not running extended tests';
};

$HINTS{ author } = sub
{
	return if $ENV{AUTHOR_TESTING};
	plan skip_all => 'Not running author tests';
};

$HINTS{ release } = sub
{
	return if $ENV{RELEASE_TESTING};
	plan skip_all => 'Not running author tests';
};

$HINTS{ interactive } = sub
{
	return unless $ENV{AUTOMATED_TESTING};
	plan skip_all => 'Not running interactive tests';
};

$HINTS{ requires } = sub
{
	my %requires = %{ $_[2] };
	for my $module (sort keys %requires)
	{
		try {
			&require_module(
				$module,
				defined($requires{$module}) ? $requires{$module} : (),
			);
		}
		catch {
			plan skip_all => sprintf(
				"Test requires %s%s",
				$module,
				defined($requires{$module}) ? " $requires{$module}" : "",
			);
		}
	}
	return;
};

our @ISA = qw(Exporter::Tiny);
our %EXPORT_TAGS = (
	more     => [qw(
		ok is isnt like unlike is_deeply cmp_ok new_ok isa_ok can_ok
		pass fail
		diag note explain
		subtest
		skip todo_skip plan done_testing BAIL_OUT
	)],
	fatal    => [qw( exception )],
	warnings => [qw( warnings warning )],
	api      => [qw( public_ok import_ok class_api_ok )],
	moose    => [qw( does_ok )],
	strings  => [qw(
		is_string is_string_nows like_string unlike_string
		contains_string lacks_string
	)],
	deep     => [qw( cmp_deeply TD )],
	deeper   => [qw(
		cmp_deeply TD
		ignore methods listmethods shallow noclass useclass
		re superhashof subhashof bag superbagof subbagof
		set supersetof subsetof all any obj_isa array_each
		str num bool code
	)],
	deprecated => [qw( use_ok require_ok eq_array eq_hash eq_set )],
	%HINTS,
);

our @EXPORT_OK = (
	'object_ok',
	map(@$_, grep { ref($_) eq 'ARRAY' } values(%EXPORT_TAGS)),
);

our @EXPORT = (
	'object_ok',
	map(@{$EXPORT_TAGS{$_}}, qw(more fatal warnings api moose strings deep)),
);

# Here we check to see if the import list consists
# only of hints. If so, we add @EXPORT to the list.
# This means that `use Test::Modern -extended;`
# won't result in the standard exports getting
# suppressed.
#
sub import
{
	my $me = shift;
	my $symbols = grep {
		ref($_)                        ? 0 :  # refs are not symbols
		/\A[:-](\w+)\z/ && $HINTS{$1}  ? 0 :  # hints are not symbols
		1;                                    # everything else is
	} @_;
	
	push @_, @EXPORT if $symbols == 0;
	
	unshift @_, $me;
	goto \&Exporter::Tiny::import;
}

sub _exporter_validate_opts
{
	shift;
	my ($opts) = @_;
	my $caller = $opts->{into};
	
	# Exporter::Tiny can't handle exporting variables
	# at the moment. :-(
	#
	{
		no strict qw(refs);
		(ref($caller) ? $caller->{'$TODO'} : *{"$caller\::TODO"})
			= \$Test::More::TODO;
	}
	
	return if ref $caller;
	'strict'->import::into($caller);
	'warnings'->import::into($caller);	
}

# Additional exports
#

sub class_api_ok ($;@)
{
	my ($package, @expected) = @_;
	my $tb    = Test::API::_builder();
	my $label = "public API for class $package";

	return 0 unless Test::API::_check_loaded( $package, $label );

	my ($ok, $missing, $extra) = Test::API::_public_ok($package, @expected);
	
	# Call ->can to check if missing methods might be provided
	# by parent classes...
	if (!$ok)
	{
		@$missing = grep { not $package->can($_) } @$missing;
		$ok       = not( scalar(@$missing) + scalar(@$extra) );
	}
	
	$tb->ok($ok, $label);
	if (!$ok)
	{
		$tb->diag("missing: @$missing") if @$missing;
		$tb->diag("extra: @$extra")     if @$extra;
	}
	return $ok;
}

# Basically just a copy of Test::More::isa_ok
sub does_ok ($$;$)
{
	my( $thing, $class, $thing_name ) = @_;
	my $tb = Test::More->builder;
	
	my $whatami;
	if( !defined $thing ) {
		$whatami = 'undef';
	}
	elsif( ref $thing ) {
		$whatami = 'reference';
		
		local($@,$!);
		require Scalar::Util;
		if( Scalar::Util::blessed($thing) ) {
			$whatami = 'object';
		}
	}
	else {
		$whatami = 'class';
	}
	
	my( $rslt, $error ) = $tb->_try( sub { $thing->DOES($class) } );
	
	if ($error) {
		die <<WHOA unless $error =~ /^Can't (locate|call) method "DOES"/;
WHOA! I tried to call ->DOES on your $whatami and got some weird error.
Here's the error.
$error
WHOA
	}
	
	# Special case for isa_ok( [], "ARRAY" ) and like
	if( $whatami eq 'reference' ) {
		$rslt = UNIVERSAL::DOES($thing, $class);
	}
	
	my($diag, $name);
	if( defined $thing_name ) {
		$name = "'$thing_name' does '$class'";
		$diag = defined $thing ? "'$thing_name' doesn't '$class'" : "'$thing_name' isn't defined";
	}
	elsif( $whatami eq 'object' ) {
		my $my_class = ref $thing;
		$thing_name = qq[An object of class '$my_class'];
		$name = "$thing_name does '$class'";
		$diag = "The object of class '$my_class' doesn't '$class'";
	}
	elsif( $whatami eq 'reference' ) {
		my $type = ref $thing;
		$thing_name = qq[A reference of type '$type'];
		$name = "$thing_name does '$class'";
		$diag = "The reference of type '$type' doesn't '$class'";
	}
	elsif( $whatami eq 'undef' ) {
		$thing_name = 'undef';
		$name = "$thing_name does '$class'";
		$diag = "$thing_name isn't defined";
	}
	elsif( $whatami eq 'class' ) {
		$thing_name = qq[The class (or class-like) '$thing'];
		$name = "$thing_name does '$class'";
		$diag = "$thing_name doesn't '$class'";
	}
	else {
		die;
	}
	
	my $ok;
	if($rslt) {
		$ok = $tb->ok( 1, $name );
	}
	else {
		$ok = $tb->ok( 0, $name );
		$tb->diag("	$diag\n");
	}
	
	return $ok;
}

sub object_ok
{
	local $Test::Builder::Level = $Test::Builder::Level + 1;
	
	my $object = shift;
	my $name   = (@_%2) ? shift : '$object';
	my %tests  = @_;
	
	subtest("$name ok", sub
	{
		if (ref($object) eq q(CODE))
		{
			try {
				my $tmp = $object->();
				die 'coderef did not return an object'
					unless ref($tmp);
				$object = $tmp;
				pass("instantiate $name");
			}
			catch {
				fail("instantiate $name");
				diag("instantiating $name threw an exception: $_");
			}
		}
		
		ok(Scalar::Util::blessed($object), "$name is blessed")
			or return;
		
		if (exists($tests{isa}))
		{
			my @classes = ref($tests{isa}) eq q(ARRAY) ? @{$tests{isa}} : $tests{isa};
			isa_ok($object, $_, $name) for @classes;
		}
		
		if (exists($tests{does}))
		{
			my @roles = ref($tests{does}) eq q(ARRAY) ? @{$tests{does}} : $tests{does};
			does_ok($object, $_, $name) for @roles;
		}
		
		if (exists($tests{can}))
		{
			my @methods = ref($tests{can}) eq q(ARRAY) ? @{$tests{can}} : $tests{can};
			can_ok($object, @methods);
		}
		
		if (exists($tests{api}))
		{
			my @methods = ref($tests{api}) eq q(ARRAY) ? @{$tests{api}} : $tests{api};
			class_api_ok(ref($object), @methods);
		}
		
		done_testing;
	});
}

sub _generate_TD
{
	my $_td = bless(do { my $x = 1; \$x }, 'Test::Modern::_TD');
	return sub () { $_td };
}

sub Test::Modern::_TD::AUTOLOAD
{
	shift;
	my ($method) = ($Test::Modern::_TD::AUTOLOAD =~ /(\w+)\z/);
	return if $method eq 'DESTROY';
	my $coderef = 'Test::Deep'->can($method)
		or die("Test::Deep::$method not found");
	$coderef->(@_);
}

1;

## no Test::Tabs

__END__

=pod

=encoding utf-8

=begin trustme

=item C<does_ok>

=item C<class_api_ok>

=end trustme

=head1 NAME

Test::Modern - commonly used test functions and features for modern Perl code

=head1 SYNOPSIS

   use Test::Modern;
   
   # Your tests here
   
   done_testing;

=head1 DESCRIPTION

Test::Modern provides the best features of L<Test::More>, L<Test::Fatal>,
L<Test::Warnings>, L<Test::API>, L<Test::LongString>, and L<Test::Deep>,
as well as ideas from L<Test::Requires>, L<Test::DescribeMe>, and
L<Test::Moose>.

Test::Modern also automatically imposes L<strict> and L<warnings> on your
script.

=head2 Features from Test::More

Test::Modern exports the following subs from L<Test::More>:

=over

=item *

C<< ok($truth, $description) >>

=item *

C<< is($got, $expected, $description) >>

=item *

C<< isnt($got, $unexpected, $description) >>

=item *

C<< like($got, $regexp, $description) >>

=item *

C<< unlike($got, $regexp, $description) >>

=item *

C<< is_deeply($got, $expected, $description) >>

=item *

C<< cmp_ok($got, $operator, $expected, $description) >>

=item *

C<< new_ok($class, \@args, $name) >>

=item *

C<< isa_ok($object|$subclass, $class, $name) >>

=item *

C<< can_ok($object|$class, @methods) >>

=item *

C<< pass($description) >>

=item *

C<< fail($description) >>

=item *

C<< subtest($description, sub { ... }) >>

=item *

C<< diag(@messages) >>

=item *

C<< note(@messages) >>

=item *

C<< explain(@messages) >>

=item *

C<< skip($why, $count) if $reason >>

=item *

C<< todo_skip($why, $count) if $reason >>

=item *

C<< $TODO >>

=item *

C<< plan(%plan) >>

=item *

C<< done_testing >>

=item *

C<< BAIL_OUT($reason) >>

=back

The C<use_ok>, C<require_ok>, C<eq_array>, C<eq_hash>, and C<eq_set> functions
are also available, but not exported by default. For C<use_ok> and
C<require_ok> it's normally better to use the Perl built-ins C<use> and
C<require> which will die (failing your test) if things are not OK. For
the C<< eq_* >> functions, they can usually be replaced by C<is_deeply>.

=head2 Features from Test::Fatal

Test::Modern exports the following subs from L<Test::Fatal>:

=over

=item *

C<< exception { BLOCK } >>

=back

=head2 Features from Test::Warnings

Test::Modern exports the following subs from L<Test::Warnings>:

=over

=item *

C<< warning { BLOCK } >>

=item *

C<< warnings { BLOCK } >>

=back

In addition, Test::Modern always enables the C<had_no_warnings> test at
the end of the file, ensuring that your test script generated no warnings
other than the expected ones which were caught by C<warnings> blocks.

=head2 Features from Test::API

Test::Modern exports the following subs from L<Test::API>:

=over

=item *

C<< public_ok($package, @functions) >>

=item *

C<< import_ok($package, @functions) >>

=back

Test::Modern also provides another test modelled after C<public_ok>:

=over

=item *

C<< class_api_ok($class, @methods) >>

Like C<public_ok>, but allows missing functions to be inherited from
parent classes. Extra methods in parent classes will not cause the
test to fail.

=back

=head2 Features from Test::LongString

Test::Modern exports the following subs from L<Test::LongString>:

=over

=item *

C<< is_string($got, $expected, $description) >>

=item *

C<< is_string_nows($got, $expected, $description) >>

=item *

C<< like_string($got, $regexp, $description) >>

=item *

C<< unlike_string($got, $regexp, $description) >>

=item *

C<< contains_string($haystack, $needle, $description) >>

=item *

C<< lacks_string($haystack, $needle, $description) >>

=back

=head2 Features from Test::Deep

Test::Modern exports the following subs from L<Test::Deep>:

=over

=item *

C<< cmp_deeply($got, $expected, $description) >>

=back

The following are not exported by default, but can be exported upon request:

=over

=item *

C<< ignore() >>

=item *

C<< methods(%hash) >>

=item *

C<< listmethods(%hash) >>

=item *

C<< shallow($thing) >>

=item *

C<< noclass($thing) >>

=item *

C<< useclass($thing) >>

=item *

C<< re($regexp, $capture_data, $flags) >>

=item *

C<< superhashof(\%hash) >>

=item *

C<< subhashof(\%hash) >>

=item *

C<< bag(@elements) >>

=item *

C<< set(@elements) >>

=item *

C<< superbagof(@elements) >>

=item *

C<< subbagof(@elements) >>

=item *

C<< supersetof(@elements) >>

=item *

C<< subsetof(@elements) >>

=item *

C<< all(@expecteds) >>

=item *

C<< any(@expecteds) >>

=item *

C<< obj_isa($class) >>

=item *

C<< array_each($thing) >>

=item *

C<< str($string) >>

=item *

C<< num($number, $tolerance) >>

=item *

C<< bool($value) >>

=item *

C<< code(\&subref) >>

=back

As an alternative to using those functions, Test::Modern exports a constant
C<TD> upon which you can call them as methods:

   # like Test::Deep::bag(@elements)
   TD->bag(@elements)

=head2 Features inspired by Test::Moose

=over

=item *

C<< does_ok($object|$subclass, $class, $name) >>

Like C<isa_ok>, but calls C<< $obj->DOES >> instead of C<< $obj->isa >>.

=back

=head2 Features inspired by Test::Requires

=over

=item *

C<< use Test::Modern -requires => \%requirements >>

This will skip the entire test script if the requirements are not met.
For example:

   use Test::Modern -requires => {
      'Moose'                => '2.11',
      'namespace::autoclean' => undef,
   };

=back

=head2 Features inspired by Test::DescribeMe

These export tags allow you to classify tests as "author tests",
"release tests", "extended tests", or "interactive tests".

They will cause your test script to be skipped depending on
various environment variables.

=over

=item *

C<< use Test::Modern -author >>

=item *

C<< use Test::Modern -release >>

=item *

C<< use Test::Modern -extended >>

=item *

C<< use Test::Modern -interactive >>

=back

=head2 Brand Spanking New Features

=over

=item *

C<< object_ok($object, $name, %tests) >>

Runs a gamut of subtests on an object:

   object_ok(
      $object,
      $name,
      isa   => \@classes,
      does  => \@roles,
      can   => \@methods,
      api   => \@methods,
   );

C<< $object >> may be a blessed object, or an unblessed coderef which
returns a blessed object. The C<< isa >> test runs C<< isa_ok >>; the
C<< does >> test runs C<< does_ok >>, the C<< can >> test runs
C<< can_ok >>, and the C<< api >> test runs C<< class_api_ok >>. Any of
the test hash keys may be omitted, in which case that test will not be
run. C<< $name >> may be omitted.

=back

=head1 EXPORT

This module uses L<Exporter::Tiny> to perform its exports. This allows
exported subs to be renamed, etc.

The following export tags are supported:

=over

=item C<< -more >>

Exports the L</"Features from Test::More">, except deprecated ones.

=item C<< -deprecated >>

Exports the deprecated Test::More features.

=item C<< -fatal >>

Exports the L</"Features from Test::Fatal">.

=item C<< -warnings >>

Exports the L</"Features from Test::Warnings">.

=item C<< -api >>

Exports the L</"Features from Test::API">, including C<class_api_ok>.

=item C<< -strings >>

Exports the L</"Features from Test::LongString">.

=item C<< -deep >>

Exports L<cmp_deeply and TD|/"Features from Test::Deep">.

=item C<< -deeper >>

Exports I<all> the L</"Features from Test::Deep">.

=item C<< -moose >>

Exports the L</"Features inspired by Test::Moose">.

=item C<< -default >>

Exports the default features -- all of the above except C<< -deprecated >>
and C<< -deeper >>.

=item C<< -all >>

Exports all of the above features I<including> C<< -deprecated >> and
C<< -deeper >>.

=item C<< -author >>, C<< -extended >>, C<< -interactive >>, and C<< -release >>

Classify the test script.

=item C<< -requires >>

Specify test requirements.

=back

C<< $TODO >> is currently I<always> exported.

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Test-Modern>.

=head1 SEE ALSO

L<Test::More>,
L<Test::Fatal>,
L<Test::Warnings>,
L<Test::API>,
L<Test::LongString>,
L<Test::Deep>,
L<Test::Moose>,
L<Test::Requires>,
L<Test::DescribeMe>.

L<Test::Most> is a similar idea, but provides a slightly different
combination of features.

L<My Favourite Test::* Modules|http://blogs.perl.org/users/toby_inkster/2014/02/my-favourite-test-modules.html>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

