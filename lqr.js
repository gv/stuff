function Loc(h) {
	h = h.split('*');
	var xScale = h.shift() || '1';
	var m = xScale.match('([0-9]+)px$');
	if(m) {
		this.width  = parseInt(m[1], 10);
	} else {
		this.xScale = parseFloat(xScale, 10);
	}
	this.url = h.join('*');
}

Loc.prototype.toString = function() {
	if(this.url) {
		var s = this.width ? (this.width + 'px') : this.xScale;
		return s + '*' + this.url;
	}
	return '';
};

var curImg;
Loc.prototype.getWidth = function() {
	if(this.width) {
		return this.width;
	}
	if(curImg) {
		return Math.floor(curImg.bs.width * this.xScale);
	}
	// ?
};

Loc.prototype.getXScale = function() {
	if(curImg && this.width) {
		return curImg.bs.width/this.width;
	}
	return this.xScale;
};


function render() {
	if(!loc.url) {
		ind.innerHTML = 'give me url';
		return;
	}
	
	ind.innerHTML = 'Rendering...';
	location.href = '#' + loc.toString();
	setTimeout(_render, 1);
}

function loadImg(url, _handle) {
	if(curImg) {
		if(curImg.url == url) {
			return _handle(curImg);
		}
		alert(curImg.bs.src + ' != ' + url);
	}

	var im = new Image();
	im.onload = function() {
		ind.innerHTML = im.width + 'x' + im.height;
		curImg = new Img(im, url);
		_handle(curImg);
	};

	ind.innerHTML = 'Loading ' + url + ' ...'; 
	im.src = url;
}
		
		
function Img(bs, url) {
	this.bs = bs;
	this.url = url;
	refCanv.height = bs.height;
	refCanv.width = bs.width;
	var c = refCanv.getContext('2d');
	c.drawImage(bs, 0, 0);

	try {
		this.imData = c.getImageData(0, 0, bs.width, bs.height);
	} catch(e) {
		alert(e);
	}
}

Img.prototype.getEnergies = function() {
	if(!this.energies) {
		var d = this.imData.data;
		var ee = new Array(d.length / 4 + 1), i = 1, p = 4, end;
		ee[0] = 0; // xxx
		for(; i < this.bs.width; i++) {
			ee[i] = 0; // xxx
			p += 4;
		}

		end = ee.length - 1; 
		var upper = 0, left = p - 4, e;
		for(; i < end; i++) {
			e = Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p] - d[left++]);
			p = ++left;

			e += Math.abs(d[p++] - d[upper++]);
			e += Math.abs(d[p++] - d[upper++]);
			e += Math.abs(d[p++] - d[upper++]);
			upper++;
			p++;
			ee[i] = e;
		}

		ee[end] = Infinity;
		this.energies = ee;
	}
	return this.energies;
};


Img.prototype.getSums = function() {
	if(!this.sums) {
		var ee = this.getEnergies(), pad = 2, sWidth = this.bs.width;
		var ss = new Array(ee.length + pad*this.bs.height/* + pad*/);
		var i = 0;  // dest index
		var si = pad; // src index
		ss[0] = ss[1] = Infinity;
		for(; si < sWidth; i++, si++) 
			ss[i] = ee[si];
		
		var rowEnd; // src index again
		var upper = 0; // dest index
		while(si < ee.length) {
			ss[i++] = Infinity;
			ss[i++] = Infinity;
			upper += 2;
			rowEnd = si + sWidth;
			for(;si < rowEnd; i++, si++, upper++)
				ss[i] = ee[si] + Math.min(ss[upper-1], ss[upper], ss[upper+1]);
		}
		this.sums = ss;
		this.sumsWidth = sWidth + pad;
	}
	return this.sums;
};

Img.prototype.getIndirectWeights = function(indexes, width) {
	// assume padded
	var ee = this.getEnergies();
	var sums = new Array(indexes.length);
	for(var i = 0; i < width; i++) 
		sums[i] = ee[indexes[i]];
	
	sums[i++] = sums[0]; // can't get sums[upper - 1] for upper = 0
	
	var rowEnd = width, upper = 1;
	while(i < indexes.length) {
		sums[i] = ee[indexes[i]] + Math.min(sums[upper - 1], sums[upper], 
			sums[upper + 1]);
		i++;
		upper++;
	}
	return sums;
};

Img.prototype.getWeights = function() {
	if(!this.weights) {
		var w = this.bs.width, ee = this.getEnergies();
		var ww = ee.concat();
		var end = w * this.bs.height;
		
		ww[w] = ee[w] + Math.min(ww[1], ww[0]);

		var i = w + 1, upper = 1;
		while(i < end) {
			ww[i] = ee[i] + Math.min(ww[upper-1], ww[upper], ww[upper + 1]);
			i++, upper++;
		}
		this.weights = ww;
	}
	return this.weights;
};
	
	
function _render() {
	loadImg('/c?' + loc.url, function(img) {
		var dWidth = loc.getWidth(), // won't change
			sWidth = img.bs.width;
		ind.innerHTML = 'Making ' + dWidth + ' from ' + sWidth + '...';
		setTimeout(function() {
			resizeAndDraw(img);
		}, 1);
	});
}

function resizeAndDraw(img) {
	var dbgOut = '';
	var height = img.bs.height;
	var dWidth = loc.getWidth(), // won't change
		sWidth = img.bs.width;

	// - scale -
	var g = new Graph(sWidth, height);
	var ups = g.ups, downs = g.downs, rights = g.rights, lefts = g.lefts;
	var ww = img.getWeights();
	
	var seamCntToFind = sWidth - dWidth;
	if(seamCntToFind < 0)
		seamCntToFind = sWidth;

	// find seamCntToFind seam start point indexes
	var bottomLine = g.rights.slice(sWidth * height - sWidth);
	// dbgOut += rights.join(' ') + ' ' + rights.length + ' ' + 
	// sWidth + 'x' + height + ' ';
	bottomLine.sort(function(l, r) {
			l = ww[l];
			r = ww[r];
			return (l < r) ? -1 : (l == r) ? 0 : 1;
		});
	
	//dbgOut += ww.join(' ');

	while(seamCntToFind--) {
		var i = bottomLine.shift(), r, l, d, next;

		//dbgOut += 'i' + i + ' w' + ww[i] + ' ';
		
		while(true) {
			// exclude [i]
			// We can come here from rights[i], lefts[i] or downs[i]
			r = rights[i];
			l = lefts[i];
			lefts[r] = l;
			rights[l] = r;

			// sign
			lefts[i] = -1;
			
			// l will be the next up for our down
			if(ww[r] < ww[l])
				l = r;
			d = downs[i];
			ups[d] = l;
			downs[l] = d;
			
			next = ups[i];
			if(-1 == next)
				break;
			r = rights[next];
			l = lefts[next];
			/*if(r > next)
				if(ww[r] < ww[next])
					next = r;
			if(l < next)
				if(ww[l] < ww[next])
				next = l;*/
			
			i = next;
		}
	}

	ind.innerHTML = 'drawing...';
	setTimeout(function() {
			draw(img, g, dWidth, dbgOut);
		}, 1);
}
		
		
var auxDispCanv = document.getElementById('auxDisplay');
function draw(img, g, dWidth, dbgOut) {
	dbgOut = dbgOut || '';
	var s = img.imData.data;
	var sWidth = img.bs.width, height = img.bs.height;
	dWidth = sWidth;
	var lefts = g.lefts, rights = g.rights, ups = g.ups, downs = g.downs;
	dispCanv.width = dispCanv.style.width = dWidth;
	dispCanv.height = dispCanv.style.height = height;
	
	// find an entry
	var i = 0, down, dp = 0, dEnd = 0, sp; 
	while(lefts[i] == -1)
		i++;

	var context = dispCanv.getContext('2d');
	var dest = context.createImageData(dispCanv.width, dispCanv.height);
	var d = dest.data;

	//dbgOut += 'x ' + downs.join(' ');
	
	do {
		//dbgOut += 'line ';
		down = downs[i];
		dEnd += dWidth * 4;
		while(dp < dEnd) {
			//dbgOut += i + ' ';
			sp = i * 4;
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = 255;
			i = rights[i];
		}
		i = down;
	} while(i != -1 && dp < d.length);

		
	context.putImageData(dest, 0, 0);

	// aux display
	auxDispCanv.width = auxDispCanv.style.width = sWidth;
	auxDispCanv.height = auxDispCanv.style.height = height;

	// find an entry
	var i = 0, down, dp = 0, dEnd = 0, sp; 
	while(lefts[i] == -1)
		i++;

	var context = auxDispCanv.getContext('2d');
	var dest = context.createImageData(auxDispCanv.width, auxDispCanv.height);
	var d = dest.data;

	//dbgOut += 'x ' + downs.join(' ');
	
	do {
		//dbgOut += 'line ';
		down = downs[i];
		var lim = sWidth;
		while(lim--) {
			//dbgOut += i + ' ';
			dp = sp = i * 4;
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = 255;
			i = rights[i];
		}
		i = down;
	} while(i != -1 && dp < d.length);

		
	context.putImageData(dest, 0, 0);

	ind.innerHTML = 'Press keys: a s';
	dbgInd.innerHTML = dbgOut;
}		
	

function loadBtnClicked() {
	var u = document.getElementById('url');
	loc.url = u.value;
	loc.xScale = 1;
	render();
	u.blur();
}
			
var refCanv = document.getElementById('ref');
var dispCanv = document.getElementById('display');
var ind = document.getElementById('indicator');
var dbgInd = document.getElementById('dbgInd');

document.onkeydown = function(ev) {
	switch(ev.keyCode) {
	case 13: // enter
	loadBtnClicked();
	break;

	case 65: // A
	case 83: // S
	if(65 == ev.keyCode)  {
		var xScale = loc.getXScale() - 0.1;
	} else {
		var xScale = loc.getXScale() + 0.1;
	}

	if(xScale < 0.1) { 
		xScale = 0.1;
	} else if(xScale > 5) {
		xScale = 5;
	}

	if(xScale != loc.getXScale()) {
		loc.width = null;
		loc.xScale = xScale;
		render();
	}
	break;

	case 75: // K
	case 76: // L
	if(75 == ev.keyCode) 
		var width = loc.getWidth() - 1;
	else 
		var width = loc.getWidth() - 1;

	if(width < 1) 
		width = 1;
	else if(width > 2000) 
		width = 2000;

	if(width != loc.getWidth()) {
		loc.xScale = null;
		loc.width = width;
		render();
	}		
	break;

	}
};

try {
	var h = location.hash.substring(1);
	var loc = new Loc(h);
	render();
	
	// bastard onHashChange
	setInterval(function() {
			var h = location.hash.substring(1);
			if(loc.toString() != h) {
				alert(h + ' != ' + loc.toString());
				loc = new Loc(h);
				render();
			}
		}, 1000);
} catch(e) {
	alert(e);
}

	
		
function Graph(width, height) {
	var lefts = new Array(width * height);
	for(var i = lefts.length - 1; i >= 0; i--) 
		lefts[i] = i - 1;
	this.lefts = lefts;	
	var rights = this.rights = lefts.slice(2).concat(lefts.length - 1, 0);
	var emptyRow = new Array(width);
	for(var i = emptyRow.length - 1; i <= 0; i--) {
		emptyRow[i] = -1;
	}
	this.ups = emptyRow.concat(lefts.slice(1, lefts.length - width + 1));
	this.downs = lefts.slice(1 + width).concat(lefts.length - 1, emptyRow);
	// wrap
	for(var start = lefts.length - width, end; start >= 0; start -= width) {
		end = start + width - 1;
		rights[end] = start; // start is right to the end
		lefts[start] = end; // end is left to the start
	}
}

