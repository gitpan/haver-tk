#!/usr/bin/perl
# vim: set ft=perl ts=4 sw=4:

# haver-tk.pl, Perl/Tk client for Haver-compatible chat servers.
# Copyright (C) 2003 Bryan Donlan
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

use strict;
use warnings;
use Carp;
BEGIN {
	$Haver::Preprocessor::ASSERT = 1;
	$Haver::Preprocessor::DUMP = 1;
	$Haver::Preprocessor::DEBUG = 1;
	$Haver::Preprocessor::VERBOSE = 1;
}
use Tk;
use POE qw(
	   Wheel::SocketFactory
	   Wheel::ReadWrite Driver::SysRW
	   Component::Client::Haver
);

use Haver::Preprocessor;
use Haver::Client::UserList;
use Haver::Config;
use Haver::OS;
use Data::Dumper;

eval { require POE::Wheel::SSLSocketFactory; };
my $enable_ssl = !$@;

# XXX: SSLSocketFactory client connections don't work yet.
$enable_ssl = 0;

our $Package = __PACKAGE__;

my $pass = '';
my ($user, $confdir, $config);
eval {
	$user = Haver::OS->current_user();
	$confdir = Haver::OS->config_find(
		scope => 'user',
		name => 'haver-tk',
		type => 'dir',
	);

	$config = Haver::Config->new(
		file => "$confdir/config",
		default => {
			UID => $ENV{USER} || '',
			Channel => 'lobby',
			HistSize => 50,
			Address => 'hardison.net:7070',
		},
	);
};

if ($@) {
	my $err = $@;
	my $mw = new MainWindow;
	print STDERR $err, "\n";
	$mw->title('Fatal error');
	$mw->Label(
		-text => "Error initializing configuration:\n$err"
	)->pack;
	$mw->Button(
		-text => 'Quit',
		-command => sub{exit}
	)->pack;
	MainLoop;
	exit;
}

my $users = new Haver::Client::UserList (
	on_add => \&ulist_add,
	on_remove => \&ulist_remove,
);
my $curchannel;
my $rawlog = 0;
my $mw;
my $frame;
my $tbox;
my $entry;
my $aj;
my $ulist;


sub tbox_print (@) {
	foreach (@_) {
		$tbox->insert( 'end', $_ . "\n" );
	}
	$tbox->yview('end');
}

#sub update_ulist {
#	$ulist->delete( 0, 'end' );
#	$ulist->insert( 0, sort @$users );
#}

sub _start {
	my ( $kernel, $session, $heap ) = @_[ KERNEL, SESSION, HEAP ];

	
	POE::Component::Client::Haver->new('haver');
	$kernel->post('haver', 'register', 'all');

	$mw	= $poe_main_window;
	$mw->title('Haver');

	$frame = $mw->Frame();

	$tbox  = $frame->Scrolled( 'ROText',  -scrollbars => 'one', -background => 'white' );
	$ulist = $frame->Scrolled( 'Listbox', -scrollbars => 'one', -background => 'white' );
	$aj	= $frame->Adjuster(-widget => $tbox, -side => 'left' );

	$tbox->pack(  -side => 'left', -fill   => 'both', -expand => 1 );
	$ulist->pack( -side => 'left', -fill   => 'both', -expand => 1 );
	$frame->pack( -fill => 'both', -expand => 1 );
	
	eval {
		require Tk::HistEntry;
	};
	if ($@) {
		$entry = $mw->Entry(-background => 'white');
	} else {	
		$entry = $mw->SimpleHistEntry(
			-limit => $config->{HistSize},
			-background => 'white',
		);
		$entry->historyMergeFromFile("$confdir/history");
	}
	$entry->bind( '<Return>', $session->postback('input'));
	$entry->pack( -fill => 'x',	-expand => 0 );

	$kernel->delay('connect_win', 0.1);
}

sub _stop {
	if ($entry->isa('Tk::HistEntry')) {
		$entry->historySave("$confdir/history");
	}
}

my ($cwin, $addrbox, $uidbox, $passbox, $channelbox, $use_ssl);

sub setup_connect_win {
	defined $cwin and return;
	my $session = shift;
	my $row = -1;
	$cwin = $mw->Toplevel();
	$cwin->title('Connect to server');
	
	$cwin->Label(-text => 'Address:', -justify => 'right')
	->grid (-column => 0, -row => ++$row);
	$addrbox = $cwin->Entry()
	->grid (-column => 1, -row => $row);
	$addrbox->insert(0, $config->{Address});


	$cwin->Label(-text => 'UID:', -justify => 'right')
	->grid (-column => 0, -row => ++$row);
	$uidbox = $cwin->Entry()
	->grid (-column => 1, -row => $row);
	$uidbox->insert(0, $config->{UID}); 
	
	$cwin->Label(-text => 'Password:', -justify => 'right')
	->grid (-column => 0, -row => ++$row);
	$passbox = $cwin->Entry(-show => '*')
	->grid (-column => 1, -row => $row);
	$passbox->insert(0, $pass);

	$cwin->Label(-text => 'Channel:', -justify => 'right')
	->grid (-column => 0, -row => ++$row);
	$channelbox = $cwin->Entry
	->grid (-column => 1, -row => $row);
	$channelbox->insert(0, $config->{Channel}); 

	$use_ssl = 0;
	if($enable_ssl){
		$cwin->Checkbutton(-text => 'Use SSL',
				   -variable => \$use_ssl)
			 ->grid(-column => 0, -row => ++$row, -columnspan => 2);
	}else{
		$cwin->Label(-text => 'SSL unavailable.',
				 -justify => 'center')
			 ->grid(-column => 0, -row => ++$row, -columnspan => 2);
	}
	
	$cwin->Button(-text => 'Connect',
		  -command => $session->postback('begin_connect'))
	->grid(-column => 0, -row => ++$row);
	
	$cwin->Button(-text => 'Quit',
		  -command => sub { $config->save(); exit })
	->grid(-column => 1, -row => $row);

	$cwin->focus;
}

sub begin_connect {
	my ($kernel, $heap) = @_[KERNEL,HEAP];
	($config->{UID} = $uidbox->get) or return;
	$pass = $passbox->get;
	$config->{Channel} = $curchannel = $channelbox->get;
	my ($host, $port) = split(/:/, ($config->{Address} = $addrbox->get));
	
	$kernel->post('haver', 'connect', 
		  UID => $config->{UID},
		  Password => $pass,
		  Host => $host,
		  Port => $port,
		  );
	tbox_print "Connecting to $config->{Address}...";
	$cwin and $cwin->destroy;
	($cwin, $addrbox, $uidbox) = ();
}

sub haver_connected {
	my $heap = $_[HEAP];
	tbox_print("Connected.\n");
	$cwin and $cwin->destroy;
	($cwin, $addrbox, $uidbox) = ();
}

sub haver_login {
	my ($kernel, $heap) = @_[KERNEL,HEAP];
	tbox_print("Logged in.");
	$pass = '';
	$kernel->post(haver => join => $curchannel);
	$heap->{ready} = 1;
}

my %commands = (
		users => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			$kernel->post(haver => users => ($args ?
							 ($args) : ($curchannel)));
			
		},
		quit => sub {
			my ($kernel, $heap) = @_[KERNEL,HEAP];
			tbox_print "[Disconnecting...]";
			$heap->{closing} = 1;
			$kernel->post(haver => 'disconnect');
		},
		make => sub {
			my ($kernel, $heap, $args) = @_[KERNEL, HEAP, ARG0];
			$kernel->post(haver => 'make' => $args);
		},
		msg => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			unless($args =~ m!
			   ( 
				 # Quoted uids, e.g:
				 # "bob the voting fish"
				 (?: [\"\'] [^\"\']+ [\"\'] ) |
				 \w+
				 )
			   \s+
			   (.+)
			   $ !x){
				tbox_print "[Syntax error. msg]";
				return;
			}
			my ($who, $msg) = ($1, $2);
			$who =~ s/^[\'\"](.+)[\'\"]$/$1/;
			$kernel->post(haver => pmsg => q{"}, $who, $msg);
			tbox_print "[To $who] $config->{UID}: $msg";
		},
		me => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			$kernel->post(haver => msg => q{:}, $curchannel, $args);
		},
		act => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			unless($args =~ m!
			   ( 
				 # Quoted uids, e.g:
				 # "bob the voting fish"
				 (?: [\"\'] [^\"\']+ [\"\'] ) |
				 \w+
				 )
			   \s+
			   (.+)
			   $ !x){
				tbox_print "[Syntax error.]";
				return;
			}
			my ($who, $msg) = ($1, $2);
			$who =~ s/^[\'\"](.+)[\'\"]$/$1/;
			$kernel->post(haver => pact => q{:}, $who, $msg);
			tbox_print "[To $who] $config->{UID} $msg";
		},
		join => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			unless($args =~ m!^ \s*
			   (
				# Quoted channel
				(?: [\"\'] [^\"\']+ [\"\'] ) |
				# Unquoted
				\w+
				)
			   \s*
			   # space fixes vim syntax highlighting...
			   $ !x ) {
				tbox_print
				q{Usage:},
				q{/join channel},
				q{/join "channel"},
				q{/join 'channel'};
			}
			my $newchannel = $1;
			$newchannel =~ s/^([\'\"]?)(.+)\1$/$2/;
			tbox_print "[Trying to join $newchannel...]";
			$kernel->post(haver => join => $newchannel);
		},
		list => sub {
			my ($kernel, $heap, $args) = @_[KERNEL,HEAP,ARG0];
			$kernel->post(haver => 'chans');
		},
		raw => sub {
			my ($kernel, $arg) = @_[KERNEL,ARG0];
			chomp $arg;
			my @args = $arg =~ m!
			(
			 # foo
			 \w+
			 |
			 # 'foo bar'
			 \'[^\']*(?:\'|$ )
			 |
			 # "He said, \"c:\\foo\""
			 \"(?:
				[^\"\\]* |
				\\.
				)*
			 (?:\"|$ )
			 )
			!gx;
			return unless(@args);
			for(@args) {
				m/^[^\'\"]/ && next;
				s/^\'([^\']*)\'$/$1/ && next;
				# Same as above detection, but with capturing
				s/\"((?:
					  [^\"\\]* |
					  \\.
					  )*)
					(?:\"|$ )
					/$1/x;
				s/\\(.)/$1/g;
			}
			$kernel->post('haver', 'send_raw', @args);
		},
		rawlog => sub {
			my ($kernel, $arg) = @_[KERNEL,ARG0];
			if($arg =~ m! ^ \s* (?:on|yes|1) \s* $!ix) {
				$rawlog = 1;
			}elsif($arg =~ m! ^ \s* (?:off|no|0) \s* $!ix) {
				$rawlog = 0;
			}else{
				tbox_print "[Usage: /rawlog on|yes|1|off|no|0]";
			}
		}
);

sub input {
	my ( $kernel, $heap ) = @_[ KERNEL, HEAP ];
	my $t = $entry->get;
	if ($entry->can('historyAdd')) {
		$entry->historyAdd($t);
	}
	$entry->delete( 0, 'end' );
	# this is not needed anymore.
	#$t =~ s/\t/' 'x8/ge;
	return if defined $heap->{closing};
	if ( !defined $heap->{ready} ) {
		tbox_print "[Not connected.]";
		return;
	}
	if ( $t =~ m!^/ (\w+) (?:\s+ (.+))? $ !x) {
		if(exists $commands{lc $1}) {
			$commands{lc $1}(@_[0..ARG0-1], $2);
		} else {
			tbox_print "[Unknown command: $1]";
		}
		return;
	}
	if($t =~ m!^ / [^ ] !x) {
		tbox_print "[Syntax error.]";
		return;
	}
	$t =~ s!^/ !!;
	$kernel->post(haver => msg => q{"}, $curchannel, $t);
}

sub haver_users { 
	my @who = @{$_[ARG0]};
	my $where = $_[ARG1];
	my $count = @who;
	tbox_print "[$count users in room $where]";
	tbox_print join(" - ", @who);

	return if $where ne $curchannel;
	$users->clear;
	foreach my $uid (@who) {
		$users->add($uid, 1);
	}	
	#update_ulist;
}

sub haver_msg {
	my @args = @{$_[ARG0]};
	my $cid = $_[ARG1];
	my ($type, $uid, $text) = @args;
	if($cid eq $curchannel) {
		if($type eq q{"}) {
			tbox_print "$uid: $_" for (split /[\r\n]+/, $text);
		} elsif ($type eq q{:}) {
			tbox_print "$uid $_" for (split /[\r\n]+/, $text);
		}
	}
}

sub haver_pmsg {
	my @args = @{$_[ARG0]};
	my ($type, $uid, $text) = @args;
	if ($type eq q{"}) {
		tbox_print "=> $uid: $_" for (split /[\r\n]+/, $text);
	} elsif ($type eq q{:}) {
		tbox_print "=> $uid $_" for (split /[\r\n]+/, $text);
	}
}

sub haver_joined {
	my ($kernel, $heap) = @_[KERNEL,HEAP];
	my $channel = $_[ARG1];
	if($channel ne $curchannel) {
		tbox_print "[Leaving $curchannel]";
		$kernel->post(haver => part => $curchannel);
	}
	tbox_print "[Joined $channel]";
	$curchannel = $channel;
	$kernel->post(haver => users => $channel);
}

sub haver_join {
	my @args = @{$_[ARG0]};
	my $cid = $_[ARG1];
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my $uid = $args[0];
	tbox_print "[$uid has entered $cid]";
	if ($cid eq $curchannel) {
		$users->add($uid, 1);
		#update_ulist;
	}
}

sub haver_part {
	my @args = @{$_[ARG0]};
	my $cid = $_[ARG1];
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my $uid = $args[0];
	tbox_print "[$uid has left $cid]";
	if ($cid eq $curchannel) {
		$users->remove($uid);
	}
	#update_ulist
}

sub haver_parted {
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my $channel = $_[ARG1];
	tbox_print "[Parted $channel]";
}

sub haver_quit {
	my @args = @{$_[ARG0]};
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my ($uid, $why) = @args;
	tbox_print "[$uid has quit: $why]";
	$users->remove($uid);
	#update_ulist;
}

sub haver_disconnected {
	my @args = @{$_[ARG0]};
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my ($enum, $etxt) = @args;
	if ($enum == 0) {
		tbox_print "[Server closes connection. Disconnected.]";
	} elsif($enum == -1) {
		tbox_print "[Disconnected]";
	} else {
		tbox_print "[Connection error: $etxt ($enum)";
	}
	delete $heap->{ready};
	delete $heap->{closing};
	&setup_connect_win($_[SESSION]);
	return;
}

sub haver_close {
	my @args = @{$_[ARG0]};
	my ($etyp, $estr) = @args;
	tbox_print "[Server closing connection: $estr]";
}

sub haver_connect_fail {
	my @args = @{$_[ARG0]};
	my ($etyp, $estr) = @args;
	tbox_print "Unable to connect to server: $estr (#$etyp)";
	&setup_connect_win($_[SESSION]);
}

sub haver_login_fail {
	my @args = @{$_[ARG0]};
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	my $estr = $args[2];
	tbox_print "Login failure: $estr";
# XXX: Redisplay login dialog
	$kernel->post(haver => 'disconnect');
	setup_connect_win($_[SESSION]);
}

sub haver_chans {
	my @chans = @{$_[ARG0]};
	my ($kernel, $heap) = @_[KERNEL,HEAP];	
	tbox_print "[Channel list:]";
	tbox_print join " - ", @chans;
}

sub haver_warn {
	my @args = @{$_[ARG0]};
	my ($err, $eshort, $elong) = @args;
	print Dumper $_[ARG0];
	tbox_print "[Warning from server: $elong]";
}

sub haver_die {
	my @args = @{$_[ARG0]};
	my ($err, $eshort, $elong) = @args[ARG0..ARG2];
	tbox_print "[Fatal error from server: $elong]";
}

sub haver_raw_in {
	my @message = @{$_[ARG0]};
	tbox_print "S: ".join "\t", @message if $rawlog;
}

sub haver_raw_out {
	my @message = @{$_[ARG0]};
	tbox_print "C: ".join "\t", @message if $rawlog;
}

sub connect_win {
	&setup_connect_win($_[SESSION]);
}

sub _default {
	my ( $kernel, $state, $event, $args, $heap ) = @_[ KERNEL, STATE, ARG0, ARG1, HEAP ];
	$args ||= [];	# Prevents uninitialized-value warnings.
	DEBUG: "default: $state = $event. Args:\n";
	DUMP: $args;
	return 0;
}

sub ulist_add {
	my ($list, $uid) = @_;
	$ulist->delete('0', 'end');
	$ulist->insert('end', sort @$list);
}

sub ulist_remove {
	my ($list, $uid) = @_;
	$ulist->delete('0', 'end');
	$ulist->insert('end', sort @$list);
}

POE::Session->create(
	package_states => [ $Package =>
		[qw(_start
		_stop
		_default
		input
		begin_connect
		connect_win
		haver_connected
		haver_login
		haver_users
		haver_msg
		haver_pmsg
		haver_joined
		haver_join
		haver_part
		haver_parted
		haver_quit
		haver_chans
		haver_disconnected
		haver_close
		haver_login_fail
		haver_connect_fail
		haver_warn
		haver_die
		haver_raw_in
		haver_raw_out
		)] ] );

POE::Kernel->run();
$config->save();
