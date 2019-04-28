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

function Pan(node, opts) {
	node = document.getElementById(node);
	this.node = node;
	this.imgs = [];
	this.loops = opts && opts.loops;
	this.viewLeft = 0;
	this.v = 0;
	this.unmeasuredCnt = 0;

	var s = this.node.style;
	s.cursor = 'move';
	s.overflow = 'hidden';
	s.position = 'relative';

	this.ind = document.createElement('DIV');
	this.node.appendChild(this.ind);
	var s = this.ind.style;
	s.position = 'absolute';
	s.zIndex = 23;
		
	// add contained IMG nodes
	
	var imgNodes = this.node.getElementsByTagName('IMG');
	// Tricky: getElementsByTagName returns some kind of "alive" collection,
	// which grows as we append child img nodes inside addNode().
	// I wonder how are we supposed to actually use those things?


	// imgNodes = Array.prototype.slice.call(imgNodes);
	// Haha, didn't work in IE. Well, to be fair, it wasn't supposed to
	// anyway. Would be nice though!

	var originalImgNodes = [];
	for(var i = 0; i < imgNodes.length; i++) {
		originalImgNodes.push(imgNodes[i]);
	}
	imgNodes = originalImgNodes;

	for(var i = 0; i < imgNodes.length; i++) {
		//alert(i + '/' + imgNodes.length);
		this.addNode(imgNodes[i]);
	}

	if(opts && opts.imgUrls) {
		for(var i in opts.imgUrls) {
			this.addImg(opts.imgUrls[i]);
		}
	}
	
	// handlers

	var pan = this, refX, refViewLeft, mouseDown;
	this.node.onmousedown = function(ev) {
		if(window.event)
			ev = window.event;
		refX = ev.clientX;
		refViewLeft = pan.viewLeft;
		mouseDown = true;
		cancel(ev);
	};
	this.node.onmouseout = function(ev) {
		//if(window.event)
		//	ev = window.event;
		//if(ev.target == pan.node)
		//	this.onmouseup(ev);
	};

	var TICKLENGTH = 40;
	var v = 0, tick = function() {
		if(mouseDown)
			return;
		var frictionAccl = Math.max(-10, Math.min(v, 10));
		v -= frictionAccl;
		pan.v = v;
		if(v)
			setTimeout(tick, 40);
		pan.viewLeft = Math.round(pan.viewLeft + v);
		pan.reposition();
	};
		

	this.node.onmouseup = function(ev) {
		if(window.event)
			ev = window.event;
		if(mouseDown) {
			move(ev.clientX);
		}
		mouseDown = false;
		cancel(ev);
		tick();
	};
	this.node.onmouseout({});


	var track = [];
	var move = function(x) {
		pan.viewLeft = refViewLeft + refX - x;
		var now = (new Date).getTime();
		if(track.length >= 2) {
			var p = track.shift();
			if(now == p.time) // events are called too often
				/*alert*/(p.time -= 1);
			v = (p.x - x)/(now - p.time)*TICKLENGTH;
			v = Math.min(230, Math.max(-230, v));
			pan.v = v;
		}
		track.push({time: now, x: x});
		pan.reposition();
	};		

	this.node.onmousemove = function(ev) {
		if(window.event)
			ev = window.event;
		if(mouseDown) {
			move(ev.clientX);
		}
		cancel(ev);
	};
	
	// iphone stuff
	
	this.node.ontouchstart = function(ev) {
		mouseDown = true;
		refX = ev.targetTouches[0].pageX;
		refViewLeft = pan.viewLeft;
	};

	this.node.ontouchmove = function(ev) { 
		ev.preventDefault();
		move(ev.targetTouches[0].pageX);
	};

	this.node.ontouchend = this.node.ontouchcancel = function(ev) {
		mouseDown = false;
		ev.preventDefault();
		tick();
	};
}

function toJson(o) {
	var s = '';
	for(var k in o) {
		if(typeof o[k] == 'number')
		//if(k.toLowerCase().indexOf('height') != -1)
			s += k + ':' + o[k] + ', ';
	}
	return s;
}

Pan.prototype.addImg = function(url) {
	var node = document.createElement('IMG'); 
	this.node.appendChild(node);
	// this triggers loading
	node.src = url;
	this.addNode(node);
};

Pan.prototype.addNode = function(node) {
	// image node can be loaded or not
	var im = {}, pan = this, url = node.src;
	im.url = url;
	im.width = 0;
	im.height = 0;
	im.node = node;
	var s = im.node.style;
	s.border = 'none';
	s.padding = 0;
	s.position = 'absolute';

	im.bNode = document.createElement('IMG');
	var s = im.bNode.style;
	s.border = 'none';
	s.padding = 0;
	s.position = 'absolute';
	this.node.appendChild(im.bNode);


	/*im.node.onload = */
	var setDims = function() {	
		var w = im.node.naturalWidth || im.node.width;
		var h = im.node.naturalHeight || im.node.height;
		//alert('onload: ' + w + ':' + h);
		if(w && h) {
			im.width = w;
			im.height = h;
			im.node.style.width = im.width + 'px';
			im.node.style.height = im.height + 'px';
			im.bNode.style.width = im.width + 'px';
			im.bNode.style.height = im.height + 'px';
			pan.unmeasuredCnt--;
			pan.reposition();
		} else {
			setTimeout(setDims, 1000);
		}
	};
	this.unmeasuredCnt++;
	setDims();
	
	im.bNode.src = url;

	this.imgs.push(im);
	pan.reposition();
};

Pan.prototype.reposition = function() {
	// get total
	var totalWidth = 0;
	for(var i in this.imgs)
		totalWidth += this.imgs[i].width;

	if(!totalWidth)
		return;

	while(this.viewLeft < 0)
		this.viewLeft += totalWidth;
	while(this.viewLeft > totalWidth)
		this.viewLeft -= totalWidth;

	var off = -this.viewLeft, maxHeight = 300, d = '';
	for(var i in this.imgs) {
		var im = this.imgs[i];
		var s = im.node.style;
		s.top = 0;
		s.left = off + 'px';
		d += ' ' + s.left;
		
		s = im.bNode.style;
		s.top = 0;
		s.left = off + totalWidth + 'px';
		d += ' b' + s.left;

		off += im.width;

		if(im.height > maxHeight) 
			maxHeight = im.height;
	}
	this.node.style.height = maxHeight + 'px';
	this.ind.innerHTML = this.viewLeft + '/' + 
	totalWidth + '/' + Math.round(this.v);// + d;
	if(this.unmeasuredCnt)
		this.ind.innerHTML += ' (' + this.unmeasuredCnt + ')';
	this.ind.style.color = '#ffffff';
}
		
		
		
	
