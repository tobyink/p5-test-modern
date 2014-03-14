=pod

=encoding utf-8

=head1 PURPOSE

Test that Test::Modern's C<< namespaces_clean >> feature works.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

use Test::Modern -requires => {
	'namespace::clean' => undef,
	'Moose'            => '2.0600',
};

{
	package Foo;
	use Scalar::Util qw(refaddr);
	use namespace::clean;
	$INC{'Foo.pm'} = __FILE__;
}

{
	package Bar;
	use Moose;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	$INC{'Bar.pm'} = __FILE__;
	sub xyz { 42 }
}

namespaces_clean('Foo', 'Bar');

done_testing;
