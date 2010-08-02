R = .9;
q = 50;
f = $('<svg width="400" height="500" version="1.1" xmlns="http://www.w3.org/2000/svg"></svg>').appendTo("body");
u = [];
for(i = 16; i; i--) {
	u.push({
		x: i + 1.5,
		y: i&1 ? 1.5 : 8.5,
		X: 0,
		Y: 0,
		w: i&1,
		l: $("<circle />").appendTo(f).attr("r", R*q)
	});
}


