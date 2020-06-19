#!/usr/bin/tclsh
proc usage {} {
	puts stderr "git-add-progress: shows progress while doing 'git add'"
	puts stderr "USAGE: git-add-progress [PATH ...]"
	exit 1
}

namespace eval gap {
	namespace eval v {}
}

proc gap::run {} {
	set takeoff [clock clicks -milliseconds]
	set d [open [concat "|git status --porcelain" $::argv] r]
	upvar #0 gap::state$d state
	array set state {paths? {} pathsM {} pathsD {} pathsT {} size 0}
	set state(takeoff) $takeoff 
	updateTime $d
	fileevent $d readable [list gap::handleRead $d]
	vwait done
	set landing [clock clicks -milliseconds]
	puts stderr "Done in [formatNum [expr $landing - $takeoff]] ms"
}

proc gap::updateTime {d} {
	upvar #0 gap::state$d state
	after 1000 gap::updateTime $d
	set t [expr {([clock clicks -milliseconds] - $state(takeoff))/1000}]
	puts -nonewline stderr "[clock format $t -format %M:%S] "
	puts -nonewline stderr "Running 'git status'...\r"
}

proc gap::formatNum {n} {
	string reverse [regsub -all "...(?=.)" [string reverse $n] "&,"]
}

proc gap::handleRead {d} {
	upvar #0 gap::state$d state
	set s [gets $d]
	if [fblocked $d] {
		return
	}
	if [regexp {^[AMDT? ]([AMDT? ]) (.+)$} $s -> mode p] {
		if {$mode != " "} {
			# TODO safer dequote
			if [catch "set q $p"  msg] {
				puts stderr "Error appending path '$s': $msg"
			} elseif {![string match $q */]} {
				lappend state(paths$mode)\
					[encoding convertfrom utf-8 $q]
				incr state(size)
			}
		}
	} elseif [regexp {^[RC] } $s] {
		# These are in the index already
	} elseif {$s != ""} {
		puts stderr "Unparsed: $s"
	}
	if [eof $d] {
		after cancel gap::updateTime $d
		puts stderr ""
		if [catch {close $d} m] {
			puts stderr "$m"
			exit 1
		}
		doGitAdd $d
	}
}

proc gap::putsTime {d} {
	upvar #0 gap::state$d state
	set t [expr {([clock clicks -milliseconds] - $state(takeoff))/1000}]
	if {$t >= 24*3600} {
		puts -nonewline stderr "[expr {$t/(24*3600)}]d "
	}
	puts -nonewline stderr\
		"[clock format $t -format %H:%M:%S -gmt 1] "
}

proc gap::doGitAdd {d} {
	upvar #0 gap::state$d state
	set count 0
	set opmax 64
	set errors {}
	set conds {
		M changing add ? adding add T changing add D removing {rm --cached}} 
	foreach {m verb command} $conds {
		set start 0
		while {$start < [llength $state(paths$m)]} {
			set paths [lrange $state(paths$m) $start [expr $start+$opmax-1]]
			puts -nonewline stderr\
				"[formatNum $count] / [formatNum $state(size)] "
			putsTime $d
			puts -nonewline stderr "$verb [llength $paths] files... "
			if [catch [concat exec git $command $paths] message] {
				puts stderr "error: $message"
				lappend errors "$m $message"
			} else {
				puts stderr "[regsub {ing$} $verb ed] $paths"
			}
			incr start $opmax
			incr count [llength $paths]
		}
	}
	if [llength $errors] {
		foreach message $errors {
			puts stderr " error: $message"
		}
		puts stderr "[llength $errors] errors"
	}
	set ::done 1
}

gap::run
