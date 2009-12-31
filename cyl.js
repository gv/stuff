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
	this.width = opts && opts.width;
	this.circular = !(this.maxAngle % 360);
	this.angle = 0;
	this.v = 0;


	var s = this.node.style;
	s.cursor = 'move';
	s.overflow = 'hidden';
	s.position = 'relative';
	if(this.width)
		s.width = this.width + 'px';
	
	this.ind = document.createElement('DIV');
	this.node.appendChild(this.ind);
	var s = this.ind.style;
	s.position = 'absolute';
	s.top = 0;
	s.left = 0;
	s.zIndex = 23;

	// blogspot stuff
	var aNodes = this.node.getElementsByTagName('A');
	for(var i = 0; i < aNodes.length; i++) {
		aNodes[i].href = '';
		aNodes[i].onclick = function() {return false};
	}

	var imgNodes = this.node.getElementsByTagName('IMG');
	imgNodes = Array.prototype.slice.call(imgNodes);
	
	for(var i in imgNodes) {
		var explicitAngle = parseInt(imgNodes[i].getAttribute('projectionAngle'), 10);
		//alert(explicitAngle);
		if(this.circular) {
			this.addNode(imgNodes[i], explicitAngle || this.maxAngle * i / imgNodes.length);
		} else {
			this.addNode(imgNodes[i], explicitAngle || this.maxAngle * i / (imgNodes.length - 1));
		}
	}


	// handlers

	var w = this, refX, refAngle, pressed;
	this.node.onmousedown = function(ev) { 
		if(window.event)
			ev = window.event;
		refX = ev.clientX;
		refAngle = w.angle;
		pressed = true;
		cancel(ev);
	};

	var TICKLENGTH = 40;
	var tick = function() {
		if(pressed)
			return;
		var frictionAccl = -Math.max(-1, Math.min(w.v, 1)); 
		w.v += frictionAccl;
		if(w.v)
			setTimeout(tick, 40);
		w.angle = Math.round(w.angle + w.v);
		w.redraw();
	};

	this.node.onmouseup = function(ev) {
		if(window.event) 
			ev = window.event;
		pressed = false;
		cancel(ev);
		tick();
	};

	this.node.onmouseout = function(ev) {
		if(window.event)
			ev = window.event;
		//if(ev.target == w.node)
		this.onmouseup(ev);
	};
	this.node.onmouseout({});

	var lastClientX, lastMoment;
	this.node.onmousemove = function(ev) {
		if(window.event)
			ev = window.event;
		if(pressed) {
			w.angle = refAngle + refX - ev.clientX;
			var now = (new Date).getTime();
			if(lastMoment) {
				if(now == lastMoment)
					lastMoment--;
				w.v = Math.round((lastClientX - ev.clientX) / (now - lastMoment) * TICKLENGTH);
				w.v = Math.min(50, Math.max(-50, w.v));
			}
			lastMoment = now;
			lastClientX = ev.clientX;
			w.redraw();
		}
		cancel(ev);
	};

}

Cyl.prototype.addNode = function(node, angle) {
	if(!this.imgs.length)
		this.angle = angle;

	var im = {}, w = this, url = node.src;
	im.angle = angle;
	im.node = node;
	im.width = 0;
	im.height = 0;
	
	var s = node.style;
	s.border = 'none';
	s.padding = 0;
	s.position = 'absolute';
	s.top = 0;
	s.left = 0;
	im.node.onload = function() {
		im.width = this.width || 400;
		im.height = this.height || 400;
		this.style.width = im.width + 'px';
		this.style.height = im.height + 'px';
		w.redraw();
	};
	im.node.onload();

	this.imgs.push(im);
	/*this.imgs.sort(function(l, r) {
			if(l.angle < r.angle)
				return -1;
			else if(l.angle == r.angle)
				return 0;
			return 1;
			});*/
};

Cyl.prototype.redraw = function() {
	if(!this.imgs.length)
		return;

	var d = '';
	
	// stop or loop at borders
	while(this.angle > this.maxAngle)
		this.angle = this.circular ? (this.angle - this.maxAngle) : this.maxAngle;
	while(this.angle < 0)
		this.angle = this.circular ? (this.angle + this.maxAngle) : 0;

	var superior = this.imgs[0], inferior = this.imgs[0], maxWidth = 0, maxHeight = 0;
	var effectiveSuperiorAngle = Infinity, effectiveInferiorAngle = -Infinity;
	var iIndex = 0, sIndex = 0;

	for(var i in this.imgs) {
		var im = this.imgs[i];
		var dist = im.angle - this.angle;
		if(im.angle >= this.angle) {
			if(im.angle < effectiveSuperiorAngle) {
				superior = im;
				sIndex = i;
				effectiveSuperiorAngle = im.angle;
			}
		} else {
			if(im.angle > effectiveInferiorAngle) {
				inferior = im;
				iIndex = i;
				effectiveInferiorAngle = im.angle;
			}
		}
		
		if(this.circular) {
			var effAngle = im.angle + this.maxAngle;
			if(effAngle < effectiveSuperiorAngle) {
				superior = im;
				sIndex = i;
				effectiveSuperiorAngle = effAngle;
			}
			effAngle = im.angle - this.maxAngle;
			if(effAngle > effectiveInferiorAngle) {
				inferior = im;
				iIndex = i;
				effectiveInferiorAngle = effAngle;
			}
		}
		// couldn't come up with anything more clever
		
		if(im.height > maxHeight)
			maxHeight = im.height;
		if(im.width > maxWidth)
			maxWidth = im.width;
	}

	var s = this.node.style;
	if(!this.width)
		s.width = maxWidth + 'px';
	s.height = maxHeight +'px';

	for(var i in this.imgs) {
		var im = this.imgs[i];
		var s = im.node.style, opacity;
		if(superior == im) {
			s.visibility = "visible";
			s.opacity = Math.sqrt(Math.sqrt((this.angle - effectiveInferiorAngle)/(effectiveSuperiorAngle - effectiveInferiorAngle)));
			opacity = s.opacity;
			s.zIndex = 2;
		} else if(inferior == im) {
			s.visibility = "visible";
			s.opacity = 1;
			s.zIndex = 1;
		} else {
			s.visibility = "hidden";
		}
	}

	// indicate
	
	this.ind.innerHTML = this.angle;
	//+ '/' + this.v + '/' + Math.round(opacity*10) + '/' + effectiveInferiorAngle + '/' + effectiveSuperiorAngle;
};
	
	

	
