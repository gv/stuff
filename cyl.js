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
	this.startIndex = opts && opts.startIndex || 0;


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

	var pressed;
	this.isPressed = function() {
		return pressed;
	};

	var origNodes = this.node.getElementsByTagName('IMG'), imgNodes = [];
	for(var i = 0; i < origNodes.length; i++) 
		imgNodes.push(origNodes[i]);
	
	for(var i in imgNodes) {
		var explicitAngle = parseInt(imgNodes[i].getAttribute('projectionAngle'), 
			10);
		//alert(explicitAngle);
		if(this.circular) {
			this.addNode(imgNodes[i], explicitAngle || 
				this.maxAngle * i / imgNodes.length);
		} else {
			this.addNode(imgNodes[i], explicitAngle || 
				this.maxAngle * i / (imgNodes.length - 1));
		}
		this.node.appendChild(imgNodes[i]);
	}


	// blogspot stuff
	var aNodes = this.node.getElementsByTagName('A');
	/*for(var i = 0; i < aNodes.length; i++) {
		aNodes[i].style.display = 'none';
		//aNodes[i].href = '';
		//aNodes[i].onclick = function() {return false};
		}*/
	for(var i = aNodes.length; i;)
		this.node.removeChild(aNodes[--i]);

	// handlers

	var w = this, refX, refAngle;
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
		// Guess offset[XY] are relative to ev.target
		if(ev.offsetX < 0 || ev.offsetY < 0 ||
			ev.offsetX >= w.node.offsetWidth || ev.offsetY >= w.node.offsetHeight)
			this.onmouseup(ev);
		// We don't actually get onmouseout()s where target = w.node, probably
		// because w.node is covered by IMGs. So we only get events for
		// IMG nodes.
	};

	var lastClientX, lastMoment;

	var move = function(x) {
		var degreesPerPixel = w.maxAngle / (w.node.offsetWidth * 0.8);
		degreesPerPixel = Math.min(1, degreesPerPixel);
		w.angle = Math.round(refAngle + (refX - x) * degreesPerPixel);
		var now = (new Date).getTime();
		if(lastMoment) {
			if(now == lastMoment)
				lastMoment--;
			w.v = Math.round((lastClientX - x) / (now - lastMoment) * TICKLENGTH);
			w.v = Math.min(30, Math.max(-30, w.v));
		}
		lastMoment = now;
		lastClientX = x;
		w.redraw();
	};

	this.node.onmousemove = function(ev) {
		if(window.event)
			ev = window.event;
		if(pressed) {
			move(ev.clientX);
		}
		cancel(ev);
	};

	// iphone stuff

	this.node.ontouchstart = function(ev) {
		// at this point we can be pretty sure it's not IE :)
		//ev.preventDefault();
		refX = ev.targetTouches[0].pageX;
		refAngle = w.angle;
	};

	this.node.ontouchmove = function(ev) {
		ev.preventDefault();
		move(ev.targetTouches[0].pageX);
	};

	this.node.ontouchend = this.node.ontouchcancel = function(ev) {
		ev.preventDefault();
		tick();
	};

}

Cyl.prototype.addNode = function(node, angle) {
	if(this.startIndex == this.imgs.length)
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
	s.cursor = "move";
	s.filter = 'alpha(opacity=100)';
	im.node.onload = function() {
		im.width = this.width || 400;
		im.height = this.height || 400;
		//alert('ol:' + this.width + ':' + this.height);
		w.redraw();
	};

	//  In case onload was called earlier.
	im.width = im.node.width;
	im.height = im.node.height;

	this.imgs.push(im);
	this.redraw();
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

	if(-Infinity == effectiveInferiorAngle)
		return;
	
	try {
	for(var i in this.imgs) {
		var im = this.imgs[i];
		var s = im.node.style, opacity;
		if(superior == im) {
			s.visibility = "visible";
			opacity = step((this.angle - effectiveInferiorAngle)/
				(effectiveSuperiorAngle - effectiveInferiorAngle));
			if(im.node.filters) {
				// im.node.filters.alpha || alert('no alpha');
				im.node.filters.alpha.opacity = opacity * 100;
			}
			s.opacity = opacity;
			s.zIndex = 2;
		} else if(inferior == im) {
			s.visibility = "visible";
			if(im.node.filters) {
				// im.node.filters.alpha || alert('no alpha');
				im.node.filters.alpha.opacity = 100;
			}
			s.opacity = 1;
			s.zIndex = 1;
		} else {
			// Interestingly, this can generate onmouseout()s for im.node,
			// which then bubble up to this.node
			s.visibility = "hidden";
		}
	}
	} catch(e) {
		alert(toJson(e) + ' | ' + opacity);
	}

	// indicate
	
	this.ind.innerHTML = this.angle + (this.isPressed() ? '/p' : '');
	//+ '/' + this.v + '/' + Math.round(opacity*10) + '/' + effectiveInferiorAngle + '/' + effectiveSuperiorAngle;
};
	
	
function step(x) {
	//alert(x);
	// [0,1] -> [0,1]
	// maybe it will look better if we spend less time in 50% opacity
	x = 2*x - 1;
	var y;
	if(x < 0) {
		y = -Math.pow(-x, 1/4);
	} else {
		y = Math.pow(x, 1/4);
	}
	y = (y + 1)/2;
	return y;
}
		
function toJson(o) {
	var s = '';
	for(var k in o) {
		//if(typeof o[k] == 'number')
		//if(k.toLowerCase().indexOf('height') != -1)
			s += k + ':' + o[k] + ', ';
	}
	return s;
}
