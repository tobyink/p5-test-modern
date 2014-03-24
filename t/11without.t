=pod

=encoding utf-8

=head1 PURPOSE

Test that Test::Modern's C<< -without >> feature works.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

use Test::Modern -without => [qw(
	CGI::Push
	LWP::UserAgent
	Net::NNTP
)];

like(
	exception { require CGI::Push },
	qr/did not return a true value/,
);

like(
	exception { require LWP::UserAgent },
	qr/did not return a true value/,
);

like(
	exception { require Net::NNTP },
	qr/did not return a true value/,
);

done_testing;
