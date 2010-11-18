DEBUG = true;
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
		var d = this.imData.data, w = this.bs.width, h = this.bs.height;
		var ee = new Array(w * h);
		var i = 1, p = 4, left = 0, e;
		for(; i < w; i++) {
			e = Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p++] - d[left++]);
			p++, left++;
			ee[i] = e; // xxx
		}

		var end = ee.length; 
		var upper = 0;
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

		//fix
		for(i = ee.length - w; i >= 0; i -= w)
			ee[i] = 255*6;

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
		var i = w, upper = 0, last;
		while(i < end) {
			last = i + w - 1
			ww[i] = ee[i] + Math.min(ww[upper], ww[upper + 1]);
			i++, upper++;
			while(i < last) {
				ww[i] = ee[i] + Math.min(ww[upper-1], ww[upper], ww[upper + 1]);
				i++, upper++;
			}
			ww[i] = ee[i] + Math.min(ww[upper], ww[upper - 1]);
			i++, upper++;
		}

		this.weights = ww;
	}
	return this.weights;
};

Img.prototype.getWins = function() {
	var w = this.bs.width, ee = this.getEnergies();
	var ww = ee.concat();
}
	
	
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

function lessNumbersFirst(l, r) {
			return (l < r) ? -1 : (l == r) ? 0 : 1;
}	

function greaterNumbersFirst(l, r) {
			return (l < r) ? 1 : (l == r) ? 0 : -1;
}	

function resizeAndDraw(img) {
	var dbgOut = '';
	var height = img.bs.height;
	var dWidth = loc.getWidth(), // won't change
		w = img.bs.width;
	var len = w * height;
	var pathWidth = w - dWidth;

	var ee = img.getEnergies();
	var ww = new Array(ee.size);
	for(var i = 0; i < w; i++) {
		ww[i] = -ee[i];
	}

	for(var i = w; i < len; i++) {
		var xx = ww.slice(i - w, i);
		xx.sort(greaterNumbersFirst);
		//dbgOut += '[' + xx.slice(0, 50) + '] ';
		var upperBound = xx[pathWidth];

		ww[i] = Math.min(upperBound, Math.max(ww[i-w], ww[i-w+1])) - ee[i];
		var end =  i + w - 1;
		for(i++; i < end; i++) {
			ww[i] = Math.min(upperBound, Math.max(ww[i-w], ww[i-w+1], ww[i-w-1])) - 
				ee[i];
		}
		ww[i] = Math.min(upperBound, Math.max(ww[i-w], ww[i-w-1])) - ee[i];
	}

	//var dbgMap = new Array(sWidth * height);
	var cutMap = new Array(w * height);
	var indexes = [];
	var lineBase = len - w;
	for(var i = len - 1; i >= lineBase; i--)
		indexes.push(i);

	indexes.sort(function(l, r) {
			l = ww[l], r = ww[r];
			return (l < r) ? 1 : (l == r) ? 0 : -1;
		});
	for(var i = pathWidth - 1; i >= 0; i--) {
		if(cutMap[indexes[i]])
			dbgOut += i + ':' + indexes[i] + ':' + indexes[i] % w + ' ';
		cutMap[indexes[i]] = i + 1;
	}
		
	for(;;) {
		lineBase -= w;
		if(lineBase < 0)
			break;
		i = lineBase + w - 1;
		if(cutMap[i + w]) {
			if(ww[i] > ww[i - 1]) {
				cutMap[i] = 9;
				ww[i] = -Infinity;
			} else {
				cutMap[i - 1] = 8;
				ww[i - 1] = -Infinity;
			}
		}
		
		for(i--; i > lineBase; i--) {
			if(cutMap[i + w]) {
				if(ww[i] > ww[i - 1]) {
					if(ww[i] > ww[i + 1]) {
						cutMap[i] = 7;
						ww[i] = -Infinity;
					} else {
						cutMap[i + 1] = 6;
						ww[i + 1] = -Infinity;
					}
				} else {
					if(ww[i - 1] > ww[i + 1]) {
						cutMap[i - 1] = 5;
						ww[i - 1] = -Infinity;
					} else {
						cutMap[i + 1] = 4;
						ww[i + 1] = -Infinity;
					}
				}
			}
		}

		if(cutMap[i + w]) {
			if(ww[i] > ww[i + 1]) {
				cutMap[i] = 3;
				ww[i] = -Infinity;
			} else {
				if(cutMap[i + 1]) 
					dbgOut += ' zasada';
				cutMap[i + 1] = 1;
				ww[i + 1] = -Infinity;
			}
		}
	}

	ind.innerHTML = 'drawing...';
	setTimeout(function() {
			draw(img, dWidth, dbgOut, cutMap, ww);
		}, 1);
}
		
		
var auxDispCanv = document.getElementById('auxDisplay');
function draw(img, dWidth, dbgOut, dbgMap, ww) {
	dbgOut = dbgOut || '';
	var s = img.imData.data;
	var sWidth = img.bs.width, height = img.bs.height;
	dispCanv.width = dispCanv.style.width = dWidth;
	dispCanv.height = dispCanv.style.height = height;
	
	var context = dispCanv.getContext('2d');
	var dest = context.createImageData(dispCanv.width, dispCanv.height);
	var d = dest.data;

	for(var sp = 0, dp = 0, i = 0; i < dbgMap.length; i++) {
		if(dbgMap[i]) {
			sp += 4;
		} else {
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
			d[dp++] = s[sp++];
		}
	}
		
	context.putImageData(dest, 0, 0);

	// aux display
	auxDispCanv.width = auxDispCanv.style.width = sWidth;
	auxDispCanv.height = auxDispCanv.style.height = height;

	var context = auxDispCanv.getContext('2d');
	var dest = context.createImageData(auxDispCanv.width, auxDispCanv.height);
	var d = dest.data;

	//dbgOut += 'x ' + downs.join(' ');

	//var ee = img.getEnergies();
	var f = -0.05;
	for(var j = dbgMap.length - 1, dp = d.length - 1; j >= 0; j--) {
		d[dp--] = 255;
		d[dp--] = f*ww[j] - 512;
		d[dp--] = f*ww[j] - 256;
		d[dp--] = f*ww[j];
		/*if(dbgMap[j]) {
			d[dp--] = 255;
			d[dp--] = 0;
			d[dp--] = dbgMap[j] % 15 * 15;
			d[dp--] = dbgMap[j] % 14 * 14;
		} else {
			d[dp--] = 255;
			d[dp--] = ee[j] - 512;
			d[dp--] = ee[j] - 256;
			d[dp--] = ee[j];
			}*/
	}
	
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
	var width = loc.getWidth() + (75 == ev.keyCode ? -1 : 1);

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
	var border = new Array(width);
	for(var i = border.length - 1; i >= 0; i--) {
		border[i] = -2;
	}
	this.ups = border.concat(lefts.slice(1, lefts.length - width + 1));
	this.downs = lefts.slice(1 + width).concat(lefts.length - 1, border);
	// wrap
	for(var start = lefts.length - width, end; start >= 0; start -= width) {
		end = start + width - 1;
		rights[end] = start; // start is right to the end
		lefts[start] = end; // end is left to the start
	}
}

ind.innerHTML = '123';
