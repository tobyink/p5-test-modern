use XT::Util;
use Test::Modern;
use Test::Pod::Coverage;

plan skip_all => __CONFIG__->{skip_all}
	if __CONFIG__->{skip_all};

if ( __CONFIG__->{modules} )
{
	my @modules = @{ __CONFIG__->{modules} };
	pod_coverage_ok($_, "$_ is covered") for @modules;
}
else
{
	all_pod_coverage_ok();
}

done_testing;
