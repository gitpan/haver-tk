#!/usr/bin/perl
use Module::Build;
my $build = Module::Build->new(
	module_name        => 'Haver::Tk',
	dist_name          => 'haver-tk',
	dist_version_from  => 'lib/Haver/Tk.pm',
	dist_author        => 'Bryan Donlan <bdonlan@bd-home-comp.no-ip.org>',
	license => 'gpl',
	requires => {
		'perl'                          => '5.6.1',
		POE                             => 0.27,
		Haver                           => 0.06,
		'Haver::Client'                 => 0.06,
		'POE::Component::Client::Haver' => 0.06,
		'Data::Dumper'                  => 2.121,
		Tk                              => 800,
	},
	recommends => {
		'Tk::HistEntry'                 => 0.42,
	},
	create_makefile_pl => 'passthrough',
	script_files       => [ glob ('bin/*.pl') ],
);

$build->create_build_script;
