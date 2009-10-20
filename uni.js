var bodies = [];
var started = false, intervalId;
var scale = 2, cam;
var POINTCNT = 200;

function start() {
	// init
	for(var i = 0; i < POINTCNT; i++) {
		if(!bodies[i]) 
			bodies[i] = {};
		var b = bodies[i];
		b.x = b.y = 0;
		// velocities are in c's
		b.vx = (Math.random() - 0.5) * 2;
		b.vy = (Math.random() - 0.5) * 2;

		if(!b.node) {
			b.node = stick();
			b.node.innerHTML = '.';
			b.node.style.fontWeight = 'bold';
			b.node.style.cursor = 'hand';
			b.node.onclick = mkClkHandler(b);
		}
	}
	if(!started) {
		intervalId = setInterval(tick, 100);
		started = true;
	}
	setCam(bodies[Math.round(Math.random()*bodies.length)]);
}

function setCam(b) {
	if(cam && cam.node) {
		cam.node.style.border = 'none';
	}
	cam = b;
	if(cam && cam.node) {
		cam.node.style.border = '1px solid red';
	}
}
	

function stick() {
	var node  = document.createElement('div');
	node.style.position = 'absolute';
	document.body.appendChild(node);
	return node;
}

function mkClkHandler(b) {
	return function() {	setCam(b); };
}

function tick() {
	var vpw = window.innerWidth, vph = window.innerHeight;
	for(var i in bodies) {
		var b = bodies[i];
		b.x += b.vx;
		b.y += b.vy;
		
		// display
		var cx = vpw / 2 + (b.x - cam.x) * scale;
		var cy = vph / 2 + (b.y - cam.y) * scale;
		if(cx < 30 || cx > vpw - 30 || cy < 30 || cy > vph - 30) {
			b.node.style.display = 'none';
		} else {
			
			b.node.style.display = '';
			b.node.style.left = cx;
			b.node.style.top  = cy;
		}
	}
}
		

