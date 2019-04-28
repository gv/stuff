#!/usr/bin/perl -w

@colors = ();
$i = 0;
while($i < 16) {
	$colors[$i] = sprintf("#%06X", int(rand(0x1000000)));
	$i++;
}

@colors = sort {
#	$aa = ord(substr($a, -2)) + ord(substr($a, -4)) + ord(substr($a, -6));
#	$bb = ord(substr($b, -2)) + ord(substr($b, -4)) + ord(substr($b, -6));

	$aa = hex(substr($a, -2)) + hex(substr($a, -4, 2)) + hex(substr($a, -6, 2));
	$bb = hex(substr($b, -2)) + hex(substr($b, -4, 2)) + hex(substr($b, -6, 2));
	return $aa <=> $bb;
} @colors;

$path = "temp-resources";
open(H, ">", $path)  || die "$0: can't open $path for writing: $!";

print(H "xterm*faceName: Liberation Mono:size=8:antialias=false\n");
print(H "xterm*vt100*geometry: 80x30\n");

if(rand(2) > 1) {
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

print(H "xterm*selectToClipboard: true\n");
system("xrdb temp-resources");
exec("xterm");
