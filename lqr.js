function Loc(h) {
	h = h.split('*');
	var xScale = h.shift() || '1';
	var m = xScale.match('([0-9]+)px$');
	if(m) {
		this.width  = parseInt(m[1], 10);
	} else {
		this.xScale = parseInt(xScale, 10);
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

Loc.prototype.getWidth = function() {
	if(this.width)
		return this.width;
	if(curImg)
		return Math.floor(curImg.bs.width * this.xScale);
	// ?
};

Loc.prototype.getXScale = function() {
	if(curImg && this.width)
		return curImg.bs.width/this.width;
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

var curImg;
function loadImg(url, _handle) {
	if(curImg) {
		if(curImg.url == url)
			return _handle(curImg);
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
	var dWidth = loc.getWidth(), // won't change
		sWidth = img.bs.width;

	// - scale -
	// init indexes
	var energies = img.getEnergies(), ws;
	var infInd = energies.length - 1;
	var indexesWidth = Math.max(sWidth + 2, dWidth);
	var indexes = new Array(img.bs.height * indexesWidth);
	var backup = new Array(img.bs.height * indexesWidth);
	var paddingEnd = indexesWidth - sWidth;
	for(var i = 0, si = 0, sRowEnd = 0; i < indexes.length;) {
		while(i < paddingEnd)
			indexes[i++] = infInd; // --> Infinity
		paddingEnd += indexesWidth;
		sRowEnd += sWidth;
		while(si < sRowEnd)
			indexes[i++] = si++;
	}
			
	
	var weightChangesCnt = 0;
	var setWeight = function(ind, val) {
		if(ws[ind] != val) {
			weightChangesCnt++;
			ws[ind] = val;
			var lower = ind + indexesWidth;
			if(lower - 1 >= ws.length)
				return;
			setWeight(lower - 1, 
				energies[indexes[lower - 1]] + 
				Math.min(ws[ind - 2], ws[ind - 1], val));
			if(lower  >= ws.length)
				return;
			setWeight(lower, 
				energies[indexes[lower]] + 
				Math.min(ws[ind + 1], ws[ind - 1], val));
			if(lower + 1 >= ws.length)
				return;
			setWeight(lower + 1, 
				energies[indexes[lower + 1]] + 
				Math.min(ws[ind + 2], ws[ind + 1], val));
		}
	};

	while(sWidth != dWidth) {
		//alert(dWidth + ' ' + sWidth);
		ws = img.getIndirectWeights(indexes, indexesWidth);
		var maxSeamCnt = sWidth - dWidth;
		if(maxSeamCnt < 0)
			maxSeamCnt = sWidth;
		
		// find no more then maxSeamCnt seams
		for(var seamCnt = 0; seamCnt < maxSeamCnt; seamCnt++) {
			weightChangesCnt = 0;
			// find a bottom point
			var minWeight = Infinity, seamInd = 0;
			var rowStart = ws.length - sWidth;
			for(var i = ws.length - 1; i >= rowStart; i--) 
				if(ws[i] < minWeight) {
					seamInd = i;
					minWeight = ws[i];
				}
			
			if(!seamInd) {
				dbgOut += seamCnt + 'seams; ';
				break;
			}

			dbgOut += ('i:' + seamInd + ' ');
			backup[seamInd] = indexes[seamInd];
			indexes[seamInd] = infInd;
			ws[seamInd] = Infinity;
			
			// go up
			while(seamInd > indexesWidth) {
				seamInd -= (indexesWidth + 1);
				if(ws[seamInd + 1] <= ws[seamInd])
					seamInd++;
				if(ws[seamInd + 1] < ws[seamInd])
					seamInd++;
				
				// ASSERT 
				if(Infinity == ws[seamInd])
					alert('BAD INDEX ' + seamInd);
				
				backup[seamInd] = indexes[seamInd];
				indexes[seamInd] = infInd;
				setWeight(seamInd, Infinity);
			}					
			dbgOut += weightChangesCnt + 'p ' + (weightChangesCnt/ws.length) + '% ';
		}


		// rearrange
		var rowEnd = 0, rowStart;
		if(sWidth > dWidth) {
			// cut
			while(rowEnd < indexes.length) {
				rowEnd += indexesWidth;
				rowStart = rowEnd - sWidth;
				for(si = i = rowEnd - 1; si >= rowStart; si--) {
					if(infInd != indexes[si]) 
						indexes[i--] = indexes[si];
				}
				rowStart = rowEnd - indexesWidth;
				while(i >= rowStart)
					indexes[i--] = infInd;
			}
			sWidth -= seamCnt;
		} else {
			// duplicate
			while(rowEnd < indexes.length) {
				rowEnd += indexesWidth;
				rowStart = rowEnd - dWidth;
				for(i = rowEnd - dWidth, si = rowEnd - sWidth; i < si; i++, si++) {
					if(infInd != indexes[si]) {
						indexes[i] = indexes[si];
					} else {
						indexes[i++] = backup[si];
						indexes[i] = backup[si];
					}
				}
			}
		}
		
	}
	
	// - draw -
	//alert('drawing:');
	var src = img.imData.data;
	dispCanv.height = dispCanv.style.height = img.bs.height;
	/*dispCanv.width = dispCanv.style.width = sWidth;
		var c = dispCanv.getContext('2d');
		var dest = c.createImageData(dispCanv.width, dispCanv.height);
		var d = dest.data, sPos;
		
		for(var si = 0, indexRowEnd = 0, dPos = 0; si < indexes.length; ) {
		indexRowEnd += indexesWidth;
		for(si = indexRowEnd - dWidth; si < indexRowEnd; si++) {
		sPos = indexes[si] * 4;
		d[dPos++] = src[sPos++];
		d[dPos++] = src[sPos++];
		d[dPos++] = src[sPos++];
		d[dPos++] = src[sPos++];
		}
		}
	*/

	dispCanv.width = dispCanv.style.width = img.bs.width;
	dispCanv.style.backgroundColor = '#bbaaff';
	var c = dispCanv.getContext('2d');
	var dest = c.createImageData(dispCanv.width, dispCanv.height);
	var d = dest.data, sPos;

	for(var si = 0, indexRowEnd = 0; si < indexes.length; ) {
		indexRowEnd += indexesWidth;
		for(si = indexRowEnd - dWidth; si < indexRowEnd; si++) {
			sPos = indexes[si] * 4;
			d[sPos] = src[sPos];
			sPos++;
			d[sPos] = src[sPos];
			sPos++;
			d[sPos] = src[sPos];
			sPos++;
			d[sPos] = src[sPos];
			sPos++;
		}
	}
			

	dbgInd.innerHTML = dbgOut;
	c.putImageData(dest, 0, 0);
	
	ind.innerHTML = 'Press keys: a s'
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

	
		
