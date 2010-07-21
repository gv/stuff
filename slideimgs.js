function SlideableImageWindow(node, srcImgs) {
	this.currentIndex = 0;
	//node = document.getElementById(node);
	this.node = node;
	var s = this.node.style;
	s.position = 'relative';
	s.minHeight = s.minWidth = '50px';
	s.cursor = 'move';
	
	this.ind = this.node.appendChild(document.createElement('DIV'));
	var s = this.ind.style;
	s.position = 'absolute';
	s.top = s.left = 0;
	s.color = 'white';
	s.zIndex = 20;
	this.indShadow = this.node.appendChild(document.createElement('DIV'));
	var s = this.indShadow.style;
	s.position = 'absolute';
	s.top = s.left = '1px';
	s.color = 'black';
	s.zIndex = 10;

	this.sleigh = this.node.appendChild(document.createElement('DIV'));
	var s = this.sleigh.style;
	s.position = 'relative';

	this.panes = [];
	do {
		var p = {};
		p.node = this.sleigh.appendChild(document.createElement('IMG'));
	} while(this.panes.push(p) < 3);

	// find images to show
	this.imgUrls = [];
	for(var i = 0; i < srcImgs.length; i++) {
		var el = srcImgs[i], url = null;
		url = el.href;
		if(url.match(imgPathPattern))
			this.imgUrls.push(url);
	}

	// install handlers
	var w = this;
	var pressed, refX;
	var move = function(x) {
		var s = w.node.style;
		s.overflow = 'hidden';
		w.sleigh.style.left = (x - refX) + 'px';
		return x - refX;
	};

	this.node.onmousedown = function(ev) {
		if(window.event)
			ev = window.event;
		pressed = true;
		refX = ev.clientX;
		cancel(ev);
	};
	
	//this.node.onmouseout = function(ev)
	this.node.onmousemove = function(ev) {
		if(pressed) {
			if(window.event)
				ev = window.event;
			move(ev.clientX);
			cancel(ev);
		}
	};

	var stop = function(x) {
		if(x < refX)
			w.currentIndex++;
		else if(x > refX)
			w.currentIndex--;
		else {
			//alert('same ' + x);
			return;
		}
		w.snap();
	};

	this.node.onmouseup = function(ev) {
		pressed = false;
		if(window.event)
			ev = window.event;
		// alert(ev.clientX + ':' + refX);
		stop(ev.clientX);
	};
		
	var refY, lastX;
	this.node.ontouchstart = function(ev) {
		pressed = true;
		lastX = refX = ev.targetTouches[0].pageX;
		refY = ev.targetTouches[0].pageY;
	};

	this.node.ontouchmove = function(ev) {
		lastX = ev.targetTouches[0].pageX;
		if(Math.abs(move(lastX)/(ev.targetTouches[0].pageY / refY)) < 10)
			;ev.preventDefault();
	};
	
	this.node.ontouchend = function(ev) {
		pressed = false;
		// Seems ev.targetTouches[0].pageX is set to value from ontouchstart
		// for some reason.
		stop(lastX);
		ev.preventDefault();
	};

	this.snap();
}

SlideableImageWindow.prototype.snap = function() {
	var len = this.imgUrls.length;
	if(this.currentIndex < 0)
		this.currentIndex += len;
	var nextIndex = this.currentIndex + 1;
	if(nextIndex >= len)
		if(nextIndex -= len)
			this.currentIndex -= len;
	var prevIndex = this.currentIndex - 1;
	if(prevIndex < 0)
		prevIndex += len;
	
	// let this.node resize itself around all that stuff
	var curUrl = this.imgUrls[this.currentIndex];
	this.sleigh.style.left = 0;
	this.panes[0].node.src = this.imgUrls[prevIndex];
	this.panes[0].node.style.display = 'none';
	this.panes[1].node.src = curUrl;
	this.panes[1].node.style.display = '';
	this.panes[2].node.src = this.imgUrls[nextIndex];
	this.panes[2].node.style.display = 'none';

	// replacing both escaped and unescaped location to be sure
	curUrl = curUrl.replace(location, '');
	this.indicate(unescape(curUrl).replace(location, '') + ' ' + 
		this.currentIndex + '/' + len);
};

	
SlideableImageWindow.prototype.indicate = function(s) {
	this.ind.innerHTML = this.indShadow.innerHTML = s;
};

imgPathPattern = new RegExp("\\.(gif|jpg|png)$", "i");

try {
	var links = document.getElementsByTagName('A'), imgLinks = [];
	for(var i = 0; i < links.length; i++) 
		if(links[i].href.match(imgPathPattern))
			imgLinks.push(links[i]);
	
	if(imgLinks.length) {
		var place = document.body.insertBefore(
			document.createElement('DIV'), document.body.firstChild);
		var s = place.style;
		s.border = '2px solid #464646';
		new SlideableImageWindow(place, imgLinks);
	}
} catch(e) {
	alert(e);
}
	

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

	


	
	
	
