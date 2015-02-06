#!/usr/bin/perl -w

@colors = (
	"#000000",
	"#555753",
	"#ff6565", 
	"#ff8d8d",
	"#93d44f",
	"#c8e7a8",
	"#eab93d",
	"#ffc123",
	"#204a87",
	"#3465a4",
	"#ce5c00",
	"#f57900",
	"#89b6e2",
	"#46a4ff",
	"#cccccc",
	"#ffffff"
	);

open(H, ">", "temp-resources");

print(H "xterm*faceName: Liberation Mono:size=10:antialias=false\n");
print(H "xterm*font: 7x13");

$i = 0;
while($i < 16) {
	$j = $i + int(rand(16 - $i));
	$c = $colors[$j];
	print(H "xterm*color${i}: $c\n");
	$colors[$j] = $colors[$i];
	$colors[$i] = $c;
	$i++;
}

print(H "xterm*background: $colors[0]\n");
print(H "xterm*foreground: $colors[7]\n");

system("xrdb temp-resources");
exec("xterm");
