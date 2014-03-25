use 5.006001;
use strict;
use warnings;

package Test::Modern;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.005';

use Exporter::Tiny   0.030 qw();
use IO::File         1.08  qw();
use IO::Handle       1.21  qw();
use Import::Into     1.002 qw();
use Module::Runtime  0.012 qw( require_module module_notional_filename );
use Test::More       0.96;
use Test::API        0.004;
use Test::Fatal      0.007;
use Test::Warnings   0.009 qw( warning warnings ), ($ENV{PERL_TEST_MODERN_ALLOW_WARNINGS} ? ':no_end_test' : ());
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
	plan skip_all => 'Not running release tests';
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
		if ($module eq 'perl')
		{
			next if !defined($requires{$module});
			next if $] >= $requires{$module};
			return plan skip_all => sprintf(
				"Test requires Perl %s",
				$requires{$module},
			);
		}
		
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

{
	my ($installed, %hide) = 0;
	
	# This implementation stolen from Devel::Hide. Keep the
	# Perl-5.6 compatible implementation, because one day it
	# might be nice if this module could support Perl 5.6.
	my $_scalar_as_io = ($] >= 5.008)
		? sub {
			open(my($io), '<', \$_[0])
				or die("Cannot open scalarref for IO?!");
			return $io;
		}
		: sub {
			my $scalar = shift;
			require File::Temp;
			my $io = File::Temp::tempfile();
			print {$io} $scalar;
			seek $io, 0, 0; # rewind the handle
			return $io;
		};
	
	my $inc = sub
	{
		my (undef, $file) = @_;
		if ($hide{$file})
		{
			my $oops = sprintf(
				qq{die "Can't locate %s (hidden by %s)\\n";},
				$file,
				__PACKAGE__,
			);
			return $_scalar_as_io->($oops);
		}
		return;
	};
	
	$HINTS{ without } = sub
	{
		unless ($installed)
		{
			unshift(@INC, $inc);
			++$installed;
		}
		
		my @without = @{ $_[2] };
		for my $module (@without)
		{
			my $file = module_notional_filename($module);
			exists($INC{$file})
				? plan(skip_all => sprintf("cannot prevent $module from loading (it is already loaded)"))
				: ++$hide{$file};
		}
		return;
	};
}

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
	pod      => [qw(
		pod_file_ok all_pod_files_ok
		pod_coverage_ok all_pod_coverage_ok
	)],
	versions => [qw( version_ok version_all_ok version_all_same )],
	strings  => [qw(
		is_string is_string_nows like_string unlike_string
		contains_string lacks_string
	)],
	clean    => [qw( namespaces_clean )],
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
	'object_ok', 'shouldnt_warn',
	map(@$_, grep { ref($_) eq 'ARRAY' } values(%EXPORT_TAGS)),
);

our @EXPORT = (
	'object_ok',
	map(@{$EXPORT_TAGS{$_}}, qw(more fatal warnings api moose strings deep clean)),
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

sub does_ok ($$;$) # just a copy of Test::More::isa_ok
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
	
	my $result = subtest("$name ok", sub
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
			delete $tests{isa};
		}
		
		if (exists($tests{does}))
		{
			my @roles = ref($tests{does}) eq q(ARRAY) ? @{$tests{does}} : $tests{does};
			does_ok($object, $_, $name) for @roles;
			delete $tests{does};
		}
		
		if (exists($tests{can}))
		{
			my @methods = ref($tests{can}) eq q(ARRAY) ? @{$tests{can}} : $tests{can};
			can_ok($object, @methods);
			delete $tests{can};
		}
		
		if (exists($tests{api}))
		{
			my @methods = ref($tests{api}) eq q(ARRAY) ? @{$tests{api}} : $tests{api};
			class_api_ok(ref($object), @methods);
			delete $tests{api};
		}
		
		if (delete($tests{clean}))
		{
			namespaces_clean(ref($object));
		}
		
		if (exists($tests{more}))
		{
			my $more = delete $tests{more};
			subtest("more tests for $name", sub
			{
				my $exception = exception { $object->$more };
				is($exception, undef, "no exception thrown by additional tests");
				done_testing;
			});
		}
		
		done_testing;
	});
	
	if (keys %tests)
	{
		my $huh = join q[, ], sort keys %tests;
		BAIL_OUT("object_ok cannot understand: $huh");
	}
	
	# return $object
	$result ? $object : ();
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

# Stolen from Test::CleanNamespaces; eliminated Package::Stash and
# Sub::Name dependencies; massively cleaned up; don't use Test::Builder
# directly; instead just call Test::More's exported functions.
#
{
	my $_dirt = sub
	{
		my $ns = shift;
		require_module($ns);

		my %symbols = do {
			no strict qw(refs);
			map   { /(\w+)$/ => $_; }
			grep  { *$_{CODE}; }
			values %{"$ns\::"};
		};
		
		my $meta;
		if ($INC{ module_notional_filename('Moose::Util') }
			and $meta = Moose::Util::find_meta($ns))
		{
			my %subs = %symbols;
			delete @subs{ $meta->get_method_list };
			return keys %subs;
		}
		elsif ($INC{ module_notional_filename('Mouse::Util') }
			and $meta = Mouse::Util::class_of($ns))
		{
			my %subs = %symbols;
			delete @subs{ $meta->get_method_list };
			return keys %subs;
		}
		else
		{
			require B;
			no strict qw(refs);
			return grep {
				my $stash = B::svref_2object(\&{"$ns\::$_"})->GV->STASH->NAME;
				$stash ne $ns
					and $stash ne 'Role::Tiny'
					and not eval { require Role::Tiny; Role::Tiny->is_role($stash) }
			} keys %symbols;
		}
	};
	
	my $_diag_dirt = sub
	{
		require B;
		
		my $ns = shift;
		my @imports = @_;
		
		my %imports;
		@imports{@imports} = map {
			B::svref_2object(\&{"$ns\::$_"})->GV->STASH->NAME . "::$_";
		} @imports;
		diag explain('remaining imports: ' => \%imports);
	};

	my $_test_or_skip = sub
	{
		my $ns = shift;
		my $rv;
		try {
			my @imports = $ns->$_dirt;
			$rv = ok(!@imports, "${ns} contains no imported functions")
				or $ns->$_diag_dirt(@imports);
		}
		catch {
			SKIP: {
				skip "failed to load $ns: $_", 1;
				fail("failed to load module");
			};
		};
		return $rv;
	};
		
	sub namespaces_clean
	{
		local $Test::Builder::Level = $Test::Builder::Level + 1;
		
		# special case a single namespace
		return shift->$_test_or_skip if @_ == 1;
		
		my @namespaces = @_;
		return subtest(
			sprintf("namespaces_clean: %s", join q(, ), @namespaces),
			sub {
				$_->$_test_or_skip for @namespaces;
				done_testing;
			},
		);
	}
}

# Release tests...
{
	sub _should_extended_test ()
	{
		$ENV{RELEASE_TESTING} || $ENV{AUTHOR_TESTING} || $ENV{EXTENDED_TESTING};
	}
	
	sub _wrap
	{
		no strict qw(refs);
		my ($module, $function, %opt) = @_;
		
		my $code;
		($function, $code) = each(%$function)
			if ref($function) eq q(HASH);
			
		*$function = sub
		{
			if ($opt{extended} and not _should_extended_test)
			{
				SKIP: {
					skip 'Not running extended tests', 1;
					pass("skipped");
				}
				return 1;
			}
			
			if (eval "require $module")
			{
				$code ||= \&{"$module\::$function"};
				if ($opt{multi})
				{
					my @args = @_;
					@_ = ($function, sub {
						@_ = @args;
						goto $code;
					});
					goto \&Test::More::subtest;
				}
				else
				{
					goto $code;
				}
			}
			
			local $Test::Builder::Level = $Test::Builder::Level + 1;
			SKIP: {
				skip "$module only required for release testing", 1
					unless $ENV{RELEASE_TESTING};
				fail("$function");
				diag("$module not installed");
			}
			return;
		};
	}
	
	my $_VAS = sub
	{
		my ($dir, $name) = @_;
		$dir
			= defined $dir ? $dir
			: -d 'blib'    ? 'blib'
			:                'lib';
		return fail("$dir does not exist, or is not a directory")
			unless -d $dir;
		
		my @files = File::Find::Rule->perl_module->in($dir);
		$name ||= "all modules in $dir have the same version number";
		
		local $Test::Builder::Level = $Test::Builder::Level + 1;
		
		subtest $name => sub
		{
			my %versions;
			for my $file (@files)
			{
				Test::Version::version_ok($file) or next;				
				my $info = Module::Metadata->new_from_file($file);
				push @{$versions{$info->version}}, $file;
			}
			my $ok = keys(%versions) < 2;
			ok($ok, "single version number found");
			if (!$ok)
			{
				diag("Files with version $_: @{$versions{$_}}")
					for sort keys(%versions);
			}
			done_testing;
		};
	};
	
	_wrap("Test::Pod", "pod_file_ok", extended => 1);
	_wrap("Test::Pod", "all_pod_files_ok", extended => 1, multi => 1);
	_wrap("Test::Pod::Coverage", "pod_coverage_ok", extended => 1);
	_wrap("Test::Pod::Coverage", "all_pod_coverage_ok", extended => 1, multi => 1);
	_wrap("Test::Version", "version_ok", extended => 1);
	_wrap("Test::Version", "version_all_ok", extended => 1, multi => 1);
	_wrap("Test::Version", { "version_all_same" => $_VAS }, extended => 1);
}

sub shouldnt_warn (&)
{
	my @warnings = do {
		local $Test::Builder::Level = $Test::Builder::Level + 3;
		&Test::Warnings::warnings(@_);
	};
	
	my $old = $TODO;
	$TODO = "shouldn't warn block";
	local $Test::Builder::Level = $Test::Builder::Level + 1;
	ok(scalar(@warnings)==0, "no (unexpected) warnings");
	diag("Saw warning: $_") for @warnings;
	$TODO = $old;
}

1;

## no Test::Tabs

__END__

=pod

=encoding utf-8

=head1 NAME

Test::Modern - precision testing for modern perl

=head1 SYNOPSIS

   use Test::Modern;
   
   # Your tests here
   
   done_testing;

=head1 DESCRIPTION

Test::Modern provides the best features of L<Test::More>, L<Test::Fatal>,
L<Test::Warnings>, L<Test::API>, L<Test::LongString>, and L<Test::Deep>,
as well as ideas from L<Test::Requires>, L<Test::DescribeMe>,
L<Test::Moose>, and L<Test::CleanNamespaces>.

Test::Modern also automatically imposes L<strict> and L<warnings> on your
script, and loads L<IO::File>. (Much of the same stuff L<Modern::Perl> does.)

Although Test::Modern is a modern testing framework, it should run fine
on pre-modern versions of Perl. It should be easy to install on Perl
5.8.9 and above; and if you can persuade its dependencies to install
(not necessarily easy!), should be OK on anything back to Perl 5.6.1.

=head2 Features from Test::More

Test::Modern exports the following subs from L<Test::More>:

=over

=item C<< ok($truth, $description) >>

=item C<< is($got, $expected, $description) >>

=item C<< isnt($got, $unexpected, $description) >>

=item C<< like($got, $regexp, $description) >>

=item C<< unlike($got, $regexp, $description) >>

=item C<< is_deeply($got, $expected, $description) >>

=item C<< cmp_ok($got, $operator, $expected, $description) >>

=item C<< new_ok($class, \@args, $name) >>

=item C<< isa_ok($object|$subclass, $class, $name) >>

=item C<< can_ok($object|$class, @methods) >>

=item C<< pass($description) >>

=item C<< fail($description) >>

=item C<< subtest($description, sub { ... }) >>

=item C<< diag(@messages) >>

=item C<< note(@messages) >>

=item C<< explain(@messages) >>

=item C<< skip($why, $count) if $reason >>

=item C<< todo_skip($why, $count) if $reason >>

=item C<< $TODO >>

=item C<< plan(%plan) >>

=item C<< done_testing >>

=item C<< BAIL_OUT($reason) >>

=back

The C<use_ok>, C<require_ok>, C<eq_array>, C<eq_hash>, and C<eq_set> functions
are also available, but not exported by default. For C<use_ok> and
C<require_ok> it's normally better to use the Perl built-ins C<use> and
C<require> which will die (failing your test) if things are not OK. For
the C<< eq_* >> functions, they can usually be replaced by C<is_deeply>.

=head2 Features from Test::Fatal

Test::Modern exports the following subs from L<Test::Fatal>:

=over

=item C<< exception { BLOCK } >>

=back

=head2 Features from Test::Warnings

Test::Modern exports the following subs from L<Test::Warnings>:

=over

=item C<< warning { BLOCK } >>

=item C<< warnings { BLOCK } >>

=back

In addition, Test::Modern always enables the C<had_no_warnings> test at
the end of the file, ensuring that your test script generated no warnings
other than the expected ones which were caught by C<warnings> blocks.
(See also C<PERL_TEST_MODERN_ALLOW_WARNINGS> in L</"ENVIRONMENT">.)

Test::Modern can also export an additional function for testing warnings,
but does not export it by default:

=over

=item C<< shouldnt_warn { BLOCK } >>

Runs a block of code that will hopefully not warn, but might. Tests that
it doesn't warn, but performs that test as a "todo" test, so if it fails,
your test suite can still pass.

=back

=head2 Features from Test::API

Test::Modern exports the following subs from L<Test::API>:

=over

=item C<< public_ok($package, @functions) >>

=item C<< import_ok($package, export => \@functions, export_ok => \@functions) >>

=item C<< class_api_ok($class, @methods) >>

=back

=head2 Features from Test::LongString

Test::Modern exports the following subs from L<Test::LongString>:

=over

=item C<< is_string($got, $expected, $description) >>

=item C<< is_string_nows($got, $expected, $description) >>

=item C<< like_string($got, $regexp, $description) >>

=item C<< unlike_string($got, $regexp, $description) >>

=item C<< contains_string($haystack, $needle, $description) >>

=item C<< lacks_string($haystack, $needle, $description) >>

=back

=head2 Features from Test::Deep

Test::Modern exports the following subs from L<Test::Deep>:

=over

=item C<< cmp_deeply($got, $expected, $description) >>

=back

The following are not exported by default, but can be exported upon request:

=over

=item C<< ignore() >>

=item C<< methods(%hash) >>

=item C<< listmethods(%hash) >>

=item C<< shallow($thing) >>

=item C<< noclass($thing) >>

=item C<< useclass($thing) >>

=item C<< re($regexp, $capture_data, $flags) >>

=item C<< superhashof(\%hash) >>

=item C<< subhashof(\%hash) >>

=item C<< bag(@elements) >>

=item C<< set(@elements) >>

=item C<< superbagof(@elements) >>

=item C<< subbagof(@elements) >>

=item C<< supersetof(@elements) >>

=item C<< subsetof(@elements) >>

=item C<< all(@expecteds) >>

=item C<< any(@expecteds) >>

=item C<< obj_isa($class) >>

=item C<< array_each($thing) >>

=item C<< str($string) >>

=item C<< num($number, $tolerance) >>

=item C<< bool($value) >>

=item C<< code(\&subref) >>

=back

As an alternative to using those functions, Test::Modern exports a constant
C<TD> upon which you can call them as methods:

   # like Test::Deep::bag(@elements)
   TD->bag(@elements)

=head2 Features from Test::Pod and Test::Pod::Coverage

B<< These features are currently considered experimental. They
may be removed from a future version of Test::Modern. >>

Test::Modern can export the following subs from L<Test::Pod> and
L<Test::Pod::Coverage>, though they are not exported by default:

=over

=item C<< pod_file_ok($file, $description) >>

=item C<< all_pod_files_ok(@dirs) >>

=item C<< pod_coverage_ok($module, $params, $description) >>

=item C<< all_pod_coverage_ok($params, $description) >>

=back

In fact, Test::Modern wraps these tests in checks to see whether
Test::Pod(::Coverage) is installed, and the state of the
C<RELEASE_TESTING>, C<AUTHOR_TESTING>, and C<EXTENDED_TESTING>
environment variables. If none of those environment variables is set to
true, then the test is skipped altogether. If Test::Pod(::Coverage) is
not installed, then the test is skipped, unless C<RELEASE_TESTING> is
true, in which case I<< Test::Pod(::Coverage) must be installed >>.

This is usually a pretty sensible behaviour. You want authors to
be made aware of pod errors if possible. You want to make sure
they are tested before doing a release. End users probably don't
want a pod formatting error to prevent them from installing the
software, unless they opt into it using C<EXTENDED_TESTING>.

Also, Test::Modern wraps the C<< all_* >> functions to run them
in a subtest (because otherwise they can interfere with your test
plans).

=head2 Features from Test::Version

B<< These features are currently considered experimental. They
may be removed from a future version of Test::Modern. >>

Test::Modern can export the following subs from L<Test::Version>,
though they are not exported by default:

=over

=item C<< version_ok($file, $description) >>

=item C<< version_all_ok(@dirs) >>

=back

These are wrapped similarly to those described in the
L</"Features from Test::Pod and Test::Coverage">.

Test::Modern can also export another sub based on C<version_all_ok>:

=over

=item C<< version_all_same(@dirs) >>

Acts like C<version_all_ok> but also checks that all modules have
the same version number.

=back

=head2 Features inspired by Test::Moose

Test::Modern does not use L<Test::Moose>, but does provide the
following function inspired by it:

=over

=item C<< does_ok($object|$subclass, $class, $name) >>

Like C<isa_ok>, but calls C<< $obj->DOES >> instead of C<< $obj->isa >>.

=back

=head2 Features inspired by Test::CleanNamespaces

Test::Modern does not use L<Test::CleanNamespaces>, but does provide
the following function inspired by it:

=over

=item C<< namespaces_clean(@namespaces) >>

Tests that namespaces don't contain any imported functions. (i.e. you
haven't forgotten to use L<namespace::autoclean> or L<namespace::sweep>
in a class).

Unlike the version of this function supplied with L<Test::CleanNamespaces>,
if C<< @namespaces >> contains more than one namespace, these will be run
in a subtest, so the whole thing will only count as one test.

=back

=head2 Features inspired by Test::Requires

Test::Modern does not use L<Test::Requires>, but does provide the
following feature inspired by it:

=over

=item C<< use Test::Modern -requires => \%requirements >>

This will skip the entire test script if the requirements are not met.
For example:

   use Test::Modern -requires => {
      'perl'                 => '5.010',
      'Moose'                => '2.11',
      'namespace::autoclean' => undef,
   };

=back

=head2 Features inspired by Test::Without::Module

Test::Modern does not use L<Test::Without::Module>, but does provide
the following feature inspired by it:

=over

=item C<< use Test::Modern -without => \@modules >>

This will run the tests as if the module was not installed. Useful
for testing things in the absence of optional dependencies. For
example:

   use Test::Modern -without => [ "Class::XSAccessor" ];

It cannot suppress modules from being loaded if they are required by
Test::Modern itself. To get a list of what modules Test::Modern
requires, run the following command:

   perl -MTest::Modern -le'print for sort keys %INC'

(Note that the actual implementation is mostly stolen from
L<Devel::Hide> which seems to behave better than
L<Test::Without::Module>.)

=back

=head2 Features inspired by Test::DescribeMe

These export tags allow you to classify tests as "author tests",
"release tests", "extended tests", or "interactive tests".

They will cause your test script to be skipped depending on
various environment variables.

=over

=item C<< use Test::Modern -author >>

=item C<< use Test::Modern -release >>

=item C<< use Test::Modern -extended >>

=item C<< use Test::Modern -interactive >>

=back

=head2 Brand Spanking New Features

Test::Modern provides a shortcut which combines several features it has
pilfered from other testing modules:

=over

=item C<< object_ok($object, $name, %tests) >>

Runs a gamut of subtests on an object:

   object_ok(
      $object,
      $name,
      isa   => \@classes,
      does  => \@roles,
      can   => \@methods,
      api   => \@methods,
      clean => $boolean,
      more  => sub {
         my $object = shift;
         ...;
      }
   );

C<< $object >> may be a blessed object, or an unblessed coderef which
returns a blessed object. The C<< isa >> test runs C<< isa_ok >>; the
C<< does >> test runs C<< does_ok >>, the C<< can >> test runs
C<< can_ok >>, and the C<< api >> test runs C<< class_api_ok >>.
C<< clean >> allows you to run C<< namespaces_clean >> on the object's
class.

C<< more >> introduces a coderef for running more tests. Within this
sub you can use any of the standard Test::More, Test::LongString, etc
tests. It is automatically run in a C<< try >> block (see L<Try::Tiny>);
throwing an exception will cause the test to fail, but not cause the
script to end.

Any of the test hash keys may be omitted, in which case that test will
not be run. C<< $name >> may be omitted.

If the test succeeds, it returns the object (which may be useful for
further tests). Otherwise, returns C<undef>.

Practical example:

   my $bob = object_ok(
      sub { Employee->new(name => 'Robert Jones') },
      '$bob',
      isa   => [qw( Employee Person Moo::Object )],
      does  => [qw( Employable )],
      can   => [qw( name employee_number tax_code )],
      clean => 1,
      more  => sub {
         my $object = shift;
         is($object->name, "Robert Jones");
         like($object->employee_number, qr/^[0-9]+$/);
      },
   );
   
   # make further use of $bob
   object_ok(
      sub { $bob->line_manager },
      isa   => [qw( Person )],
   );

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

=item C<< -clean >>

Exports the L</"Features inspired by Test::CleanNamespaces">.

=item C<< -pod >>

Exports the L</"Features from Test::Pod and Test::Pod::Coverage">.

=item C<< -versions >>

Exports the L</"Features from Test::Version">.

=item C<< -default >>

Exports the default features -- all of the above except C<< -deprecated >>,
C<< -pod >>, C<< -versions >>, and C<< -deeper >>. Also exports C<object_ok>.

=item C<< -all >>

Exports all of the above features I<including> C<< -deprecated >>,
C<< -pod >>, C<< -versions >>, C<< -deeper >>, C<object_ok>, and
C<shouldnt_warn>.

=item C<< -author >>, C<< -extended >>, C<< -interactive >>, and C<< -release >>

Classify the test script.

=item C<< -requires >>, C<< -without >>

Specify modules required or hidden for these test cases.

=back

C<< $TODO >> is currently I<always> exported.

=head1 ENVIRONMENT

Test::Modern is affected by the following environment variables:

=over

=item C<AUTHOR_TESTING>, C<AUTOMATED_TESTING>, C<EXTENDED_TESTING>, C<RELEASE_TESTING>

These variables affect the behaviour of Test::Modern's pod-checking and
version-checking. See L</"Features from Test::Pod and Test::Coverage">
and L</"Features from Test::Version">.

They also can trigger certain import tags to skip a test script. See
L</"Features inspired by Test::DescribeMe">.

=item C<PERL_TEST_MODERN_ALLOW_WARNINGS>

Setting this to true allows you to disable L<Test::Warnings>' end test.

Normally the end test will cause a test script to fail if any unexpected
warnings are encountered during its execution. New versions of Perl, and
upgrades of dependencies can cause a previously good test suite to start
emitting warnings. This environment variable can be used as a "quick fix"
to get the test suite passing again.

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Test-Modern>.

=head1 SEE ALSO

L<My Favourite Test::* Modules|http://blogs.perl.org/users/toby_inkster/2014/02/my-favourite-test-modules.html>,
L<Precision Testing for Modern Perl|http://blogs.perl.org/users/toby_inkster/2014/03/precision-testing-for-modern-perl.html>.

L<Test::More>,
L<Test::Fatal>,
L<Test::Warnings>,
L<Test::API>,
L<Test::LongString>,
L<Test::Deep>,
L<Test::Moose>,
L<Test::CleanNamespaces>,
L<Test::Requires>,
L<Test::Without::Module>,
L<Test::DescribeMe>,
L<Test::Pod>,
L<Test::Pod::Coverage>,
L<Test::Version>.

L<Test::Most> is a similar idea, but provides a slightly different
combination of features.

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

