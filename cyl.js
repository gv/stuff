function cancel(e){
	if(window.event)
		e=window.event;
	if(e.preventDefault){
		e.preventDefault();
		e.stopPropagation();
		}else{
		e.returnValue=false;
		e.cancelBubble=true;
	}
}


function Cyl(node, opts) {
	node = document.getElementById(node);
	this.node = node;
	this.imgs = []; 
	this.maxAngle = opts ? opts.maxAngle : 360;
	this.circular = (360 == this.maxAngle);
	this.angle = 0;

	var s = this.node.style;
	s.cursor = 'move';
	s.overflow = 'hidden';
	s.position = 'relative';
	
	this.ind = document.createElement('DIV');
	this.node.appendChild(this.ind);
	var s = this.ind.style;
	s.position = 'absolute';
	s.zIndex = 23;
	

	var imgNodes = this.node.getElementsByTagName('IMG');
	imgNodes = Array.prototype.slice.call(imgNodes);
	
	if(this.circular) {
		for(var i in imgNodes)
			this.addNode(imgNodes[i], this.maxAngle * i / imgNodes.length);
	} else {
		for(var i in imgNodes)
			this.addNode(imgNodes[i], this.maxAngle 8 i / (imgNodes.length - 1));
	}


	// handlers
	this.v = 0;

	var w = this, refX, refAngle, pressed;
	this.node.onmousedown = function(ev) { 
		if(window.event)
			ev = window.event;
		refX = ev.clientX;
		refAngle = w.angle;
		pressed = true;
		cancel(ev);
	};

	this.node.onmouseout = function(ev) {
		if(window.event)
			ev = window.event;
		if(ev.target == w.node)
			this.onmouseup(ev);
	};
	this.node.onmouseout({});

	var TICKLENGTH = 40;
	var tick = function() {
		if(pressed)
			return;
		var frictionAccl = -Math.max(-10, Math.min(v, 10)); 
		w.v += frictionAccl;
		if(w.v)
			setTimeout(tick, 40);
		w.angle = Math.round(w.angle + v);
		w.redraw();
	};

	this.node.onmouseup = function(ev) {
		if(window.event) 
			ev = window.event;
		pressed = false;
		cancel(ev);
		tick();
	};

	var lastClientX, lastMoment;
	this.node.onmousemove = function(ev) {
		if(window.event)
			ev = window.event;
		if(pressed) {
			w.angle = refAngle + refX - ev.clientX;
			var now = (new Date).getTime();
			
