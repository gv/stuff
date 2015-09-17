#!/usr/bin/perl -w

@colors = ();
$i = 0;
while($i < 16) {
	$colors[$i] = sprintf("#%06X", int(rand(0x1000000)));
	$i++;
}
@colors = sort @colors;

$path = "temp-resources";
open(H, ">", $path)  || die "$0: can't open $path for writing: $!";

print(H "xterm*faceName: Liberation Mono:size=9:antialias=false\n");
print(H "xterm*vt100*geometry: 80x50\n");

if(rand(2) > 0) {
	@colors = reverse @colors;
}

$i = 0;
while($i < 16) {
	$c = $colors[$i];

	print(H "xterm*color${i}: $c\n");
	if($i == 0) {
		print(H "xterm*background: $c\n");
	} 
	if($i == 15) {
		print(H "xterm*foreground: $c\n");
	}
	$i++;
}

system("xrdb temp-resources");
exec("xterm");
