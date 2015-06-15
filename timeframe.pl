#!/usr/bin/perl -w
use Time::HiRes qw( usleep ualarm gettimeofday tv_interval nanosleep
		      clock_gettime clock_getres clock_nanosleep clock
                      );

my $CPUHEAD = "user nice system idle iowait irq softirq steal guest guest_nice";

sub snap {
	my %P;
	$P{TIME} = gettimeofday;

	open(my $st, "<", "/proc/stat")  or die "cannot open: $!";
	while(<$st>) {
		my @vv = split(" ");
		my $name = shift(@vv);
		
		if($name =~ /cpu[0-9]+/) {
			my $cpucnt = 0;
			foreach(split(" ", $CPUHEAD)) {
				$P{"$name-$_"} = shift(@vv);
			}
		}
	}

	close($st);
	return %P;
}

sub subtract {
	my ($A, $B) = @_;

	foreach(keys(%$A)) {
		$elapsed = $B->{$_} - $A->{$_};
		if($elapsed != 0) {
			print("$_ = $elapsed\n");
		}
	}
}

open(my $ms, "<", "/dev/input/mice") or die "cannot open: $!";
my $last = "";
my ($c, %A, %B);

while(read($ms, $c, 1)) {
	$last .= $c;
	$last = substr($last, -3);
	
	if($last eq "\x{8}\0\0") {
		%A = snap;
	}

	if($last eq "\x{9}\0\0") {
		%B = snap;
		subtract(\%A, \%B);
		last;
	}
}


