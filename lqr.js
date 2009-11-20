function Loc(h) {
	h = h.split('*');
	var xScale = h.shift() || 1;
	var m = xScale.match('([0-9]+)px$');
	if(m) {
		this.width  = parseInt(m[1], 10);
	} else {
		this.xScale = parseInt(xScale, 10);
	}
	this.url = h.join('*');
}

Loc.prototype.toString = function() {
	if(this.width) 
		var s = this.width + 'px';
	else 
		var s = this.xScale;
	return s + '*' + this.url;
};

Loc.prototype.getWidth = function() {
	if(this.width)
		return this.width;
	if(curImg)
		return curImg.bs.width * this.xScale;
	// ?
}

function render() {
	if(!loc.url) {
		ind.innerHTML = 'give me url';
		return;
	}
	
	ind.innerHTML = 'Rendering...';
	location.href = '#' + loc.toString();
	setTimeout(_render, 1);
}

var curImg;
function loadImg(url, _handle) {
	if(curImg && curImg.bs.src == url)
		return _handle(curImg);

	var im = new Image();
	im.onload = function() {
		ind.innerHTML = im.width + 'x' + im.height;
		curImg = new Img(im);
		_handle(curImg);
	};

	ind.innerHTML = 'Loading ' + url + ' ...'; 
	im.src = url;
}
		
		
function Img(bs) {
	this.bs = bs;
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
		var ee = new Array(d.length / 4), i = 1, p = 4, end;
		ee[0] = 0; // xxx
		for(; i < this.bs.width; i++) {
			ee[i] = 0; // xxx
			p += 4;
		}

		end = ee.length; 
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
		this.energies = ee;
	}
	return this.energies;
};


Img.prototype.getSums = function() {
	if(!this.sums) {
		var ee = this.getEnergies();
		var ss = new Array(ee.length), i = 0, s;
		for(; i < this.bs.width; i++) 
			ss[i] = ee[i];

		ss[i] = Math.min(ee[0], ee[1]);
		i++;
		
		var upper = 1;
		for(; i < ss.length; i++) {
			ss[i] = ee[i] + Math.min(ss[upper-1], ss[upper], ss[upper+1]);
			upper++;
		}
		this.sums = ss;
	}
	return this.sums;
};


function _render() {
	loadImg('/c?' + loc.url, function(img) {
			var scaledWidth = Math.floor(img.bs.width * loc.xScale);
 			dispCanv.width = dispCanv.style.width = 
				img.bs.width;
			dispCanv.height = dispCanv.style.height = img.bs.height;
			
			var c = dispCanv.getContext('2d'), s = '';
			var dest = c.createImageData(img.bs.width, dispCanv.height);
			var sums = img.getSums(), d = dest.data;
			var srcWidth = img.bs.width;
			var setInd = function(i, val) {
				if(sums[i] != val) {
					var minDownVal = energies[i] + sums[i];
					sums[i] = val;
					var ni = i + width - 1;
					
					setInd(ni++, minDownVal);
					setInd(ni++, minDownVal);
					setInd(ni++, minDownVal);
				}
			};

			var src = img.imData.data;
			s = sums.length + ': ' + sums.join(' ');

			var excessColCnt = img.bs.width - scaledWidth;
			ind.innerHTML = excessColCnt;

			
			var lastRowStart = sums.length - img.bs.width;
			var visMap = new Array(sums.length);

			for(var maskedCnt = 0; maskedCnt < excessColCnt; visLvl++) {
				var minSum = Infinity;
				for(var i = sums.length - 1; i >= lastRowStart; i--) {
					if(!visMap[i]) {
						if(sums[i] < minSum) {
							minSum = sums[i];
							minInd = i;
						}
					}
				}
				if(Infinity == minSum)
					break;
				s += minInd + '=' + minSum + ' ';

				//sums[minInd] = Infinity;
				var rowStart = lastRowStart;
				do {
					setInd(minInd, Infinity);
					visMap[minInd] = maskedCnt + 1;

					minInd -= (img.bs.width + 1);
					//rowStart -= img.bs.width;
					if(minInd < 0) {
						if(minInd < -1) 
							break;
						minInd = 0;
					}
									
					if(sums[minInd+1] <= sums[minInd]) // kinda prefer to go up
						minInd++;
					if(sums[minInd+1] < sums[minInd])
						minInd++;
				} while(1);
			}
					
				
			for(var i = 0, sp = 0, dp = 0; i < sums.length; i++) {
				if(!visMap[i]) {
					d[dp++] = sums[i];
					d[dp++] = sums[i];
					d[dp++] = sums[i];
					d[dp++] = 255;
					/*d[dp++] = src[sp++];
					d[dp++] = src[sp++];
					d[dp++] = src[sp++];
					d[dp++] = src[sp++];*/
				} else {
					sp += 4;
					d[dp++] = 0;
					d[dp++] = 255;
					d[dp++] = 0;
					d[dp++] = 255;
					}
			}
			
			dbgInd.innerHTML = s;
			c.putImageData(dest, 0, 0);

			ind.innerHTML = 'Press keys: a s'
		});
	
}

function loadBtnClicked() {
	loc.url = document.getElementById('url').value;
	loc.xScale = 1;
	render();
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

	
		
