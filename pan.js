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
	this.loops = opts.loops;
	this.viewLeft = 0;
	if(opts.imgUrls) {
		for(var i in opts.imgUrls) {
			this.addImg(opts.imgUrls[i]);
		}
	}

	var s = this.node.style;
	s.cursor = 'move';
	s.overflow = 'hidden';
	s.position = 'relative';

	this.ind = document.createElement('div');
	this.node.appendChild(this.ind);
	var s = this.ind.style;
	s.position = 'absolute';
	s.zIndex = 23;
		

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
		if(window.event)
			ev = window.event;
		if(ev.target == pan.node)
			this.onmouseup(ev);
	};

	this.node.onmouseup = function(ev) {
		if(window.event)
			ev = window.event;
		mouseDown = false;
		cancel(ev);
	};
	this.node.onmouseout({});

	this.node.onmousemove = function(ev) {
		if(window.event)
			ev = window.event;
		if(mouseDown) {
			pan.viewLeft = refViewLeft + refX - ev.clientX;
			pan.reposition();
		}
		cancel(ev);
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
	var im = {}, pan = this;
	im.url = url;
	im.width = 0;
	im.height = 0;
	im.node = document.createElement('IMG');
	var s = im.node.style;
	s.border = 'none';
	s.padding = 0;
	this.node.appendChild(im.node);
	im.node.onload = function() {	
		//alert(toJson(this));
		im.width = this.width || 480;
		im.height = this.height || 480;
		this.style.width = im.width + 'px';
		this.style.height = im.height + 'px';
		pan.reposition();
	};

	var s = im.node.style;
	s.position = 'absolute';

	this.imgs.push(im);
	// this triggers loading
	im.node.src = url;
};

Pan.prototype.reposition = function() {
	var off = -this.viewLeft, maxHeight = 300, d = '';
	for(var i in this.imgs) {
		var im = this.imgs[i];
		var s = im.node.style;
		s.top = 0;
		s.left = off + 'px';
		d += ' ' + s.left;
		off += im.width;
		if(im.height > maxHeight) 
			maxHeight = im.height;
	}
	this.node.style.height = maxHeight + 'px';
	this.ind.innerHTML = this.viewLeft;
}
		
		
		
	
